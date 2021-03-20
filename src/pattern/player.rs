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

//! Play patterns using the [`scheduler`](crate::scheduler) module.
//!
//! This module implements a [`Job`](crate::scheduler::Job) plays patterns using the
//! [`scheduler`](crate::scheduler) module.
//!
//! At a high level, this module converts the [events yielded by patterns](crate::pattern::Event)
//! into timestamped events that can be sent to the SuperCollider server in OSC bundles.
//! Specifically, it converts the relative, floating point "delta" values on
//! [`pattern::Event`](crate::pattern::Event) into the [`SystemTime`] values given to the
//! `EventHandler::handle` method. The player uses a [`Tempo`] to convert the deltas into concrete
//! [`Duration`] values, then uses the logical time provided by a
//! [`Scheduler`](crate::scheduler::Scheduler) to turn those durations into [`SystemTime`] values.
//!
//! Once the event has been given a timestamp, the player sends the event to an [`EventHandler`].
//! The `EventHandler` can do whatever it wants with the event, but it usually involves converting
//! the event into a server command then sending it to the SuperCollider server in a bundle with
//! the timestamp.
//!
//! The scheduler runs the player on every beat. On each beat the player will process all of the
//! events that occur during that beat and will immediately send them to this player's
//! [`EventHandler`]. This means that changes made to patters as they are playing will only be
//! heard after the next downbeat.
//!
//! # Examples
//!
//! ```no_run
//! use sorceress::{
//!     pattern::{player::Player, sequence, Pattern},
//!     scheduler::Scheduler,
//! };
//! use std::time::SystemTime;
//!
//! let pattern: Pattern<String> = sequence(|s| {
//!     for _ in 0..4 {
//!         s.play(2.0, "2.0");
//!         s.play(1.0, "1.0");
//!         s.play(0.5, "0.5");
//!         s.play(0.5, "0.5");
//!     }
//! });
//!
//! let event_handler = |_, event| println!("{}", event);
//! Scheduler::new().run(Player::new(pattern, event_handler))?;
//! # sorceress::scheduler::Result::Ok(())
//! ```

use crate::pattern::{self, EventOrRest, Pattern};
use crate::scheduler::{Job, JobDef};
use serde::{Deserialize, Serialize};
use std::time::{Duration, SystemTime};

/// A musical tempo.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub struct Tempo {
    beat_duration: Duration,
}

impl Default for Tempo {
    /// Returns a tempo of 120 BPM (beats per minute).
    fn default() -> Tempo {
        Tempo::from_bpm(120.0)
    }
}

impl Tempo {
    /// Create a tempo from a BPM (beats per minute).
    ///
    /// # Panics
    ///
    /// This constructor will panic if bpm is not finite, zero, negative, or results in a beat
    /// duration that overflows [`Duration`].
    ///
    /// # Examples
    ///
    /// ```
    /// use sorceress::pattern::player::Tempo;
    /// use std::time::Duration;
    ///
    /// assert_eq!(Tempo::from_bpm(60.0), Tempo::from(Duration::from_secs(1)));
    /// assert_eq!(Tempo::from_bpm(120.0), Tempo::from(Duration::from_millis(500)));
    /// ```
    pub fn from_bpm(bpm: f64) -> Tempo {
        let beat_duration = Duration::from_secs_f64(1.0 / bpm * 60.0);
        Tempo { beat_duration }
    }
}

impl From<Duration> for Tempo {
    /// Create a tempo from a beat duration.
    fn from(beat_duration: Duration) -> Tempo {
        Tempo { beat_duration }
    }
}

/// Processes the events yielded by the player.
///
/// In most cases, implementors of this trait will create some SuperCollider server commands from
/// the event data, place them in a timestamped [`Bundle`](crate::server::Bundle) using the time
/// from the event, and send them to the server using
/// [`Server::send`](crate::server::Server::send).
///
/// As a convenience, a blanket implementation of this trait is provided for closures implementing
/// `FnMut(SystemTime, T)`.
///
/// # Examples
///
/// ```
/// use sorceress::{
///     pattern::player::EventHandler,
///     server::{Bundle, Control, Server, SynthNew},
/// };
/// use std::time::SystemTime;
///
/// struct ExamplesEventHandler {
///     server: Server,
/// }
///
/// impl EventHandler<f32> for ExamplesEventHandler {
///     fn handle(&mut self, time: SystemTime, event: f32) {
///         let command = SynthNew::new("test", 0).controls(vec![Control::new("freq", event)]);
///         if let Err(err) = self.server.send(Bundle::new(time, vec![command])) {
///             println!("error: sending bundle to server: {}", err);
///         }
///     }
/// }
/// ```
pub trait EventHandler<T> {
    /// Handles the event.
    fn handle(&mut self, time: SystemTime, event: T);
}

impl<T, F> EventHandler<T> for F
where
    F: FnMut(SystemTime, T),
{
    fn handle(&mut self, time: SystemTime, event: T) {
        self(time, event)
    }
}

/// A scheduler job definition for playing patterns.
///
/// Implements the [`JobDef`](crate::scheduler::JobDef) trait allowing it to executed with
/// [`Scheduler::run`](crate::scheduler::Scheduler::run).
#[derive(Debug)]
pub struct Player<T, E> {
    event_handler: E,
    pattern: Pattern<T>,
    tempo: Tempo,
}

impl<T, E> Player<T, E>
where
    E: EventHandler<T>,
{
    /// Create a `Player` for a pattern using an event handler.
    ///
    /// # Arguments
    ///
    /// * `pattern` - The pattern to play.
    /// * `event_handler` - The event handler that will be send each of the events yielded by the
    ///   pattern.
    pub fn new(pattern: Pattern<T>, event_handler: E) -> Player<T, E> {
        Player {
            event_handler,
            pattern,
            tempo: Tempo::default(),
        }
    }

    /// Set the tempo of the player.
    ///
    /// The scheduler runs the player on every beat. Each beat the player will process all of the
    /// events for that beat and send them to this player's [`EventHandler`].
    pub fn tempo(mut self, tempo: Tempo) -> Self {
        self.tempo = tempo;
        self
    }
}

impl<T, E> JobDef for Player<T, E>
where
    E: EventHandler<T>,
{
    type Job = PlayerJob<T, E>;

    fn init(self, logical_time: SystemTime) -> Option<Self::Job> {
        self.restore(logical_time, PlayerSnapshot { position: 0.0 })
    }

    fn restore(self, logical_time: SystemTime, snapshot: PlayerSnapshot) -> Option<Self::Job> {
        PlayerJob {
            event_handler: self.event_handler,
            iter: self.pattern.into_iter(),
            next_event_time: logical_time,
            position: snapshot.position,
            tempo: self.tempo,
        }
        .fast_forward_to_now()
    }
}

/// The job created by a [`Player`].
///
/// _See [`Player`] for more information._
#[derive(Debug)]
pub struct PlayerJob<T, E> {
    event_handler: E,
    iter: pattern::IntoIter<T>,
    next_event_time: SystemTime,
    position: f64,
    tempo: Tempo,
}

impl<T, E> PlayerJob<T, E> {
    fn fast_forward_to_now(mut self) -> Option<Self> {
        let mut position = 0.0;
        while position < self.position {
            position += self.iter.next()?.delta;
        }
        self.fast_forward_position(position);
        Some(self)
    }

    fn fast_forward_position(&mut self, position: f64) {
        let position_adjustment = position - self.position;
        self.next_event_time += self.tempo.beat_duration.mul_f64(position_adjustment);
        self.position = position;
    }
}

impl<T, E> Job for PlayerJob<T, E>
where
    E: EventHandler<T>,
{
    type Snapshot = PlayerSnapshot;

    fn run(&mut self, this_beat_time: SystemTime) -> Option<Duration> {
        let next_beat_time = this_beat_time + self.tempo.beat_duration;

        while self.next_event_time < next_beat_time {
            let pattern::Event { event, delta } = self.iter.next()?;
            if let EventOrRest::Event(event) = event {
                self.event_handler.handle(self.next_event_time, event);
            }
            self.next_event_time += self.tempo.beat_duration.mul_f64(delta);
            self.position += delta;
        }

        Some(self.tempo.beat_duration)
    }

    fn snapshot(self) -> PlayerSnapshot {
        PlayerSnapshot {
            position: self.position,
        }
    }
}

/// A snapshot of the interal state of a [`Player`].
///
/// Returned by [`PlayerJob::snapshot`]. [`Scheduler`](crate::scheduler::Scheduler) uses snapshots
/// to save and restore the state of a `Player` during live-reloading.
#[derive(Debug, Serialize, Deserialize)]
pub struct PlayerSnapshot {
    position: f64,
}
