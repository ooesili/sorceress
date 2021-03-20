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

//! An ahead-of-time scheduler for executing recurring jobs.
//!
//! This module provides a scheduler for running jobs that send events to the SuperCollider server
//! at, or ahead of, specific times. A [`Scheduler`] creates a [`Job`] from a [`JobDef`] and
//! invokes [`Job::run`] on that job continuously. The interval at which the job is run is
//! determined by the [`Duration`] returned by `Job::run`.
//!
//! # Examples
//!
//! ```no_run
//! use sorceress::scheduler::Scheduler;
//! use std::time::Duration;
//!
//! let mut beats = (0..4).into_iter();
//! Scheduler::new().run(|_| {
//!     move |_| {
//!         let beat = beats.next()?;
//!         println!("beat {}", beat);
//!         Some(Duration::from_millis(500))
//!     }
//! })?;
//! # sorceress::scheduler::Result::Ok(())
//! ```
//!
//! # Ephemeral Jobs
//!
//! If a job does not need to be snapshotted and restored during live-reloading it can `()` as its
//! [`Snapshot`](Job::Snapshot) type. As a convenience, these jobs can be implemented as closure
//! that takes the initial time and returns a Job, as seen in the example above.
//!
//! # Logical Time
//!
//! The functions [`JobDef::init`] and [`Job::run`] both take a `logical_time` argument. The
//! logical time is kept by the scheduler and provides a time that jobs can use as a basis for
//! timestamps on OSC bundles sent to SuperCollider. The logical time is distinctly different from
//! the _actual time_ that jobs can observe using [`SystemTime::now()`](std::time::SystemTime::now)
//! in a few ways:
//!
//! * Logical time only advances when [`Job::run`] returns a duration, and the logical time always
//!   advances by _exactly_ that duration. The logical time is never affected by fluctuations in
//!   system performance.
//!
//! * The `logical_time` is usually, and is by default, ahead of the actual time that the job is
//!   run. This delay allows time for OSC bundles to be sent to and processed by the SuperCollider
//!   server in advance of when the OSC bundles need to be scheduled. This delay can be configured
//!   using [`Scheduler::ahead_by`].
//!
//! * The logical time and actual time will vary slightly on top of the `ahead_by` delay due to the
//!   imprecise nature of the [`std::thread::sleep`] function used by the scheduler. This amount is
//!   on the order of 10ths of milliseconds on a modern desktop computer. If OSC messages are
//!   timestamped and sent to the SuperCollider server before they need to be scheduled this slight
//!   variation in time will be of no consequence.

use serde::{de::DeserializeOwned, Serialize};
use std::{
    env, io,
    time::{Duration, SystemTime},
};
use thiserror::Error;

mod scheduler_impl;

pub use scheduler_impl::Handle;

/// A specialized [`Result`] type for scheduler errors.
///
/// Most of the functions and methods that can fail in this module return this type.
pub type Result<T> = std::result::Result<T, Error>;

/// The error type returned by [`Scheduler`] operations.
#[derive(Debug, Error)]
#[error(transparent)]
pub struct Error(scheduler_impl::Error);

/// An ahead-of-time scheduler for executing recurring tasks.
#[derive(Debug)]
pub struct Scheduler(scheduler_impl::Scheduler);

impl Scheduler {
    /// Create a new scheduler
    pub fn new() -> Scheduler {
        Scheduler(scheduler_impl::Scheduler::new())
    }

    /// Schedule the job in advance.
    ///
    /// OSC messages sent to the SuperCollider server can have timestamps attached to them which
    /// lets SuperCollider execute the messages at a precise time. In this scenario the messages
    /// must be sent to the server _before_ they need to be executed.
    ///
    /// This function pushes the `logical_time` parameter passed to [`JobDef::init`] and
    /// [`Job::run`] into the future so that the OSC messages have time to be sent to the server
    /// and processed before they need to be scheduled.
    ///
    /// Defaults to 100 milliseconds.
    pub fn ahead_by(self, ahead_by: Duration) -> Self {
        Scheduler(self.0.ahead_by(ahead_by))
    }

    /// Returns a [`Handle`] which can be used to stop a [`Scheduler`] from another thread.
    ///
    /// The [`Scheduler::run`] method will return immediately after [`Handle::cancel`] is called on
    /// the handle returned by this method.
    pub fn handle(&self) -> Handle {
        self.0.handle()
    }

    /// Creates the job and runs it until it returns `None`.
    ///
    /// This function will block until the job has finished running. See [`Scheduler::handle`] for
    /// a way to interrupt this method from a different thread.
    pub fn run(self, job_def: impl JobDef) -> Result<()> {
        if self.is_live_reloading_enabled() {
            let controller = scheduler_impl::LiveController::empty()
                .read_from(io::stdin())
                .write_to(io::stdout());
            self.0.run(controller, job_def).map_err(Error)
        } else {
            let controller = scheduler_impl::OneShotController::new();
            self.0.run(controller, job_def).map_err(Error)
        }
    }

    fn is_live_reloading_enabled(&self) -> bool {
        env::var("SORCERESS_MAGIC_TOKEN")
            .map(|token| token == "7is1rzohsEbtujaufKVwfs")
            .unwrap_or(false)
    }
}

/// A [`Job`] factory.
///
/// Job definitions are given to [`Scheduler::run`] so that the jobs can be initialized and started
/// some time after `run` is called. The live-reloading functionality provided by sorceress
/// utilizes this delayed initialization.
pub trait JobDef {
    /// The type of job that this job definitions creates.
    type Job: Job;

    /// Initialize a new job.
    ///
    /// The `logical_time` argument will always be the same as what is given to the first
    /// invocation of [`Job::run`] on the returned job.
    fn init(self, logical_time: SystemTime) -> Option<Self::Job>;

    /// Restore a job from a snapshot.
    ///
    /// The `logical_time` argument will always be the same as what is given to the first
    /// invocation of [`Job::run`] on the returned job.
    fn restore(
        self,
        logical_time: SystemTime,
        snapshot: <Self::Job as Job>::Snapshot,
    ) -> Option<Self::Job>;
}

/// A recurring job.
pub trait Job {
    type Snapshot: Serialize + DeserializeOwned;

    /// Run job using the given `logical_time` as a basis for scheduling events.
    ///
    /// See [the module level documentation](self) for more.
    fn run(&mut self, logical_time: SystemTime) -> Option<Duration>;

    /// Convert the job into a serializable snapshot.
    ///
    /// This same value will be passed to the [`JobDef::restore`] method during live-reloading when
    /// the scheduler transfers control of a job to a new process.
    fn snapshot(self) -> Self::Snapshot;
}

impl<F> Job for F
where
    F: FnMut(SystemTime) -> Option<Duration>,
{
    type Snapshot = ();

    fn run(&mut self, logical_time: SystemTime) -> Option<Duration> {
        self(logical_time)
    }

    fn snapshot(self) {}
}

impl<D, J> JobDef for D
where
    D: FnOnce(SystemTime) -> J,
    J: Job<Snapshot = ()>,
{
    type Job = J;

    fn init(self, logical_time: SystemTime) -> Option<J> {
        Some(self(logical_time))
    }

    fn restore(self, logical_time: SystemTime, _snapshot: ()) -> Option<J> {
        self.init(logical_time)
    }
}
