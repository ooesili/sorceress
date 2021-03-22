// Sorceress
// Copyright (C) 2021  Wesley Merkel
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

use crate::scheduler::{Job, JobDef};
use serde::{Deserialize, Serialize};
use std::{
    io::{self, BufRead, Read, Write},
    sync::mpsc::{self, Receiver, Sender},
    thread,
    time::{Duration, SystemTime, SystemTimeError},
};
use thiserror::Error;

pub type Result<T> = std::result::Result<T, Error>;

pub trait Controller {
    type EventSink: EventSink;

    fn start(self, message_sender: Sender<Result<Message>>) -> Self::EventSink;
}

pub trait EventSink {
    fn emit(&mut self, event: Event) -> Result<()>;
}

#[derive(Debug)]
pub struct LiveController<R, W> {
    read: R,
    write: W,
}

impl LiveController<(), ()> {
    pub fn empty() -> LiveController<(), ()> {
        LiveController {
            read: (),
            write: (),
        }
    }
}

impl<R, W> LiveController<R, W> {
    pub fn read_from<T: Read>(self, read: T) -> LiveController<T, W> {
        LiveController {
            read,
            write: self.write,
        }
    }

    pub fn write_to<T: Write>(self, write: T) -> LiveController<R, T> {
        LiveController {
            read: self.read,
            write,
        }
    }
}

impl<R, W> Controller for LiveController<R, W>
where
    R: Read + Send + 'static,
    W: Write,
{
    type EventSink = ExternalEventSink<W>;

    fn start(self, message_sender: Sender<Result<Message>>) -> ExternalEventSink<W> {
        spawn_command_forwarder(message_sender, self.read);
        ExternalEventSink { write: self.write }
    }
}

pub struct ExternalEventSink<W> {
    write: W,
}

impl<W: Write> EventSink for ExternalEventSink<W> {
    fn emit(&mut self, event: Event) -> Result<()> {
        let event = serde_json::to_string(&event.0).map_err(Error::EventSerialize)?;
        writeln!(self.write, "{}", event).map_err(Error::EventWrite)?;
        Ok(())
    }
}

/// A handle to a scheduler.
///
/// Returned by [`Scheduler::handle`](super::Scheduler::handle). `Handle` primarily provides
/// [`Handle::cancel`] which cancels the scheduler that created it.
pub struct Handle {
    sender: Sender<Result<Message>>,
}

impl Handle {
    /// Cancel the scheduler that created this handle.
    pub fn cancel(self) {
        let _ = self
            .sender
            .send(Ok(Message(MessageInner::Command(Command::Quit))));
    }
}

#[derive(Debug)]
pub struct OneShotController(());

impl OneShotController {
    pub fn new() -> Self {
        OneShotController(())
    }
}

impl Controller for OneShotController {
    type EventSink = OneShotControllerEventSink;

    fn start(self, message_sender: Sender<Result<Message>>) -> OneShotControllerEventSink {
        OneShotControllerEventSink { message_sender }
    }
}

pub struct OneShotControllerEventSink {
    message_sender: Sender<Result<Message>>,
}

impl EventSink for OneShotControllerEventSink {
    fn emit(&mut self, event: Event) -> Result<()> {
        let command = match event.0 {
            EventInner::Ready => Command::Start { snapshot: None },
            EventInner::Stopped { .. } => Command::Quit,
        };
        let _ = self
            .message_sender
            .send(Ok(Message(MessageInner::Command(command))));
        Ok(())
    }
}

#[derive(Debug)]
pub struct Scheduler {
    ahead_by: Duration,
    receiver: Receiver<Result<Message>>,
    sender: Sender<Result<Message>>,
}

impl Default for Scheduler {
    fn default() -> Scheduler {
        let (sender, receiver) = mpsc::channel();
        Scheduler {
            ahead_by: Duration::from_millis(100),
            receiver,
            sender,
        }
    }
}

impl Scheduler {
    pub fn ahead_by(mut self, ahead_by: Duration) -> Self {
        self.ahead_by = ahead_by;
        self
    }

    pub fn handle(&self) -> Handle {
        let sender = self.sender.clone();
        Handle { sender }
    }

    pub fn run<C, J>(self, controller: C, job_def: J) -> Result<()>
    where
        C: Controller,
        J: JobDef,
    {
        let delay_sender = spawn_delay(self.sender.clone());
        let mut event_sink = controller.start(self.sender);

        event_sink.emit(Event(EventInner::Ready))?;

        let mut state = State::Ready { job_def };
        let mut engine = Engine {
            ahead_by: self.ahead_by,
            delay_sender,
            event_sink,
        };

        loop {
            let message = self
                .receiver
                .recv()
                .unwrap_or(Ok(Message(MessageInner::Command(Command::Quit))))?;
            state = match engine.handle(state, message)? {
                Some(state) => state,
                None => {
                    return Ok(());
                }
            };
        }
    }
}

#[derive(Debug)]
pub struct Message(MessageInner);

#[derive(Debug)]
enum MessageInner {
    DelayPassed,
    Command(Command),
}

#[derive(Debug, Serialize, Deserialize)]
enum Command {
    Start { snapshot: Option<SnapshotJSON> },
    Stop,
    Quit,
}

#[derive(Debug)]
pub struct Event(EventInner);

#[derive(Debug, Serialize, Deserialize)]
enum EventInner {
    Ready,
    Stopped { snapshot: SnapshotJSON },
}

#[derive(Debug, Serialize, Deserialize)]
struct SnapshotJSON {
    json: serde_json::Value,
    logical_time: SystemTime,
}

struct Engine<S> {
    ahead_by: Duration,
    delay_sender: Sender<Duration>,
    event_sink: S,
}

impl<S: EventSink> Engine<S> {
    fn handle<D, J>(&mut self, state: State<D, J>, message: Message) -> Result<Option<State<D, J>>>
    where
        D: JobDef<Job = J>,
        J: Job,
    {
        Ok(match (state, message.0) {
            (State::Ready { job_def }, MessageInner::Command(Command::Start { snapshot })) => self
                .initialize_job(job_def, snapshot)?
                .map(|run_state| self.run_job(run_state))
                .transpose()?
                .flatten()
                .map(State::Running),

            (State::Running(run_state), MessageInner::DelayPassed) => {
                self.run_job(run_state)?.map(State::Running)
            }

            (State::Running(run_state), MessageInner::Command(Command::Stop)) => {
                self.stop(run_state)?;
                Some(State::Stopped)
            }

            (_, MessageInner::Command(Command::Quit)) => None,

            (state, _) => Some(state),
        })
    }

    fn initialize_job<D, J>(
        &mut self,
        job_def: D,
        snapshot: Option<SnapshotJSON>,
    ) -> Result<Option<RunState<J>>>
    where
        D: JobDef<Job = J>,
        J: Job,
    {
        Ok(match snapshot {
            Some(SnapshotJSON { json, logical_time }) => {
                let snapshot = serde_json::from_value(json).map_err(Error::SnapshotDeserialize)?;
                job_def
                    .restore(logical_time, snapshot)
                    .map(|job| RunState { job, logical_time })
            }
            None => {
                let logical_time = self.now();
                job_def
                    .init(logical_time)
                    .map(|job| RunState { job, logical_time })
            }
        })
    }

    fn now(&self) -> SystemTime {
        SystemTime::now() + self.ahead_by
    }

    fn run_job<J: Job>(&mut self, mut run_state: RunState<J>) -> Result<Option<RunState<J>>> {
        run_state
            .job
            .run(run_state.logical_time)
            .map(|logical_delay| {
                run_state.logical_time += logical_delay;
                self.delay_until(run_state.logical_time)?;
                Ok(run_state)
            })
            .transpose()
    }

    fn stop<J: Job>(&mut self, run_state: RunState<J>) -> Result<()> {
        let RunState { job, logical_time } = run_state;
        let json = serde_json::to_value(job.snapshot()).map_err(Error::SnapshotSerialize)?;
        let snapshot = SnapshotJSON { json, logical_time };
        self.event_sink
            .emit(Event(EventInner::Stopped { snapshot }))?;
        Ok(())
    }

    fn delay_until(&mut self, logical_time: SystemTime) -> Result<()> {
        let delay = logical_time
            .duration_since(self.now())
            .map_err(Error::DelayTooShort)?;
        self.delay_sender.send(delay).unwrap();
        Ok(())
    }
}

#[derive(Debug)]
enum State<D, J> {
    Ready { job_def: D },
    Running(RunState<J>),
    Stopped,
}

#[derive(Debug)]
struct RunState<J> {
    job: J,
    logical_time: SystemTime,
}

fn spawn_command_forwarder<R>(sender: Sender<Result<Message>>, read: R)
where
    R: Read + Send + 'static,
{
    thread::spawn(move || {
        let lines = io::BufReader::new(read).lines();
        for line in lines {
            let message = line
                .map_err(Error::CommandRead)
                .and_then(|line| serde_json::from_str(&line).map_err(Error::CommandDeserialize))
                .map(|m| Message(MessageInner::Command(m)));
            let read_failed = message.is_err();
            let send_failed = sender.send(message).is_err();
            if read_failed || send_failed {
                break;
            }
        }
    });
}

fn spawn_delay(message_sender: Sender<Result<Message>>) -> Sender<Duration> {
    let (delay_sender, delays) = mpsc::channel();
    thread::spawn({
        move || {
            for delay in delays {
                thread::sleep(delay);
                if message_sender
                    .send(Ok(Message(MessageInner::DelayPassed)))
                    .is_err()
                {
                    break;
                }
            }
        }
    });
    delay_sender
}

#[derive(Debug, Error)]
pub enum Error {
    #[error("failed to deserialize command: {0}")]
    SnapshotDeserialize(serde_json::Error),
    #[error("failed to serialize snapshot: {0}")]
    SnapshotSerialize(serde_json::Error),
    #[error("delay returned by job is too short, puts us in the past")]
    DelayTooShort(SystemTimeError),
    #[error("failed to deserialize command: {0}")]
    CommandDeserialize(serde_json::Error),
    #[error("failed to serialize event: {0}")]
    EventSerialize(serde_json::Error),
    #[error("failed to read command from input stream: {0}")]
    CommandRead(io::Error),
    #[error("failed to write event to output stream: {0}")]
    EventWrite(io::Error),
}
