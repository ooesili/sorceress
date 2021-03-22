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

//! A module for defining patterns.
//!
//! This module provides an expressive API for describing sequences of events. A [`Pattern`] is a
//! collection of events with timing information. Patterns can be embedded into other paterns to
//! form larger sequences or can be combined in parallel to allow complex sections of music to be
//! decomposed into small independent sequences.
//!
//! # Examples
//!
//! ```
//! use sorceress::pattern::{sequence, Event, Pattern};
//!
//! let pattern: Pattern<String> = sequence(|s| {
//!     s.play(1.0, "hello");
//!     s.rest(1.0);
//!     s.parallel(|p| {
//!         p.play(2.0, "there");
//!         p.sequence(|s| {
//!             s.play(1.0, "my");
//!             s.play(1.0, "friend");
//!         });
//!     });
//! });
//!
//! let mut events = pattern.into_iter();
//!
//! assert_eq!(Some(Event::new(1.0, "hello")), events.next());
//! assert_eq!(Some(Event::rest(1.0)), events.next());
//! assert_eq!(Some(Event::new(0.0, "there")), events.next());
//! assert_eq!(Some(Event::new(1.0, "my")), events.next());
//! assert_eq!(Some(Event::new(1.0, "friend")), events.next());
//! assert_eq!(None, events.next());
//! ```
//!
//! # Splitting Up Patterns
//!
//! If you would like to split up of a large pattern for the sake of readability or reuse, you
//! should create a function that returns a [`Pattern`] and embed it into the larger pattern.
//! Prefer this over creating functions that take [`Sequence`] or [`Parallel`] types as arguments.
//!
//! ```
//! use sorceress::pattern::{parallel, Pattern, Sequence};
//!
//! fn section() -> Pattern<String> {
//!     parallel(|p| {
//!         // Do this.
//!         p.embed(chord());
//!
//!         // Not this.
//!         p.sequence(bass);
//!     })
//! }
//!
//! // Do this.
//! fn chord() -> Pattern<String> {
//!     parallel(|p| {
//!         p.play(4.0, "F4");
//!         p.play(4.0, "A5");
//!         p.play(4.0, "C5");
//!     })
//! }
//!
//! // Not this.
//! fn bass(s: &mut Sequence<String>) {
//!     s.play(1.0, "E2");
//!     s.play(1.0, "C2");
//!     s.play(2.0, "F2");
//! }
//! ```

use std::iter::{self, Peekable};

pub mod player;

/// A collection of events with timing information.
///
/// Patterns can be created using the [`sequence`] and [`parallel`] functions. Patterns can be
/// turned into flat sequences of events using [`Pattern::into_iter`]. See [the module level
/// documentation](self) for more.
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Pattern<M>(PatternInner<M>);

#[derive(Debug, Clone, PartialEq, PartialOrd)]
enum PatternInner<M> {
    Event(Event<M>),
    Parallel(Vec<Pattern<M>>),
    Sequence(Vec<Pattern<M>>),
}

/// An occurrence with timing information.
///
/// Events are used in Patterns to describe things that occur at a specific time. Events are most
/// commonly used to describe musical events such as a note playing or a pitch changing, but can be
/// used to describe any occurrence. The only information that is required by all events is a
/// `delta` which determines the time delay until the next event in a sequence.
///
/// # Examples
///
/// ```
/// use sorceress::pattern::{Event, EventOrRest};
///
/// let event = Event::new(1.0, "hello");
/// let rest = Event::rest(2.0);
///
/// assert_eq!(
///     event,
///     Event {
///         delta: 1.0,
///         event: EventOrRest::Event("hello".to_owned()),
///     }
/// );
/// assert_eq!(
///     rest,
///     Event::<String> {
///         delta: 2.0,
///         event: EventOrRest::Rest,
///     }
/// );
/// ```
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Event<M> {
    /// The "logical time" delay until the next event. Instead of being specified as a
    /// [`Duration`](std::time::Duration), `delta` is a floating point number. This lets whatever
    /// schedules the event determine how the `delta` value should be intepreted. For example: a
    /// tempo-aware scheduler can intepret the delta as a number of beats.
    pub delta: f64,

    /// The event data or a musical rest.
    pub event: EventOrRest<M>,
}

impl<M> Event<M> {
    /// Create a new event.
    pub fn new(delta: f64, event: impl Into<M>) -> Event<M> {
        Event {
            delta,
            event: EventOrRest::Event(event.into()),
        }
    }

    /// Create a rest event.
    pub fn rest(delta: f64) -> Event<M> {
        Event {
            delta,
            event: EventOrRest::Rest,
        }
    }
}

/// Represents either the data of an event or a musical rest.
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum EventOrRest<M> {
    Event(M),
    Rest,
}

/// Create a new sequence of events.
///
/// The given closure will be passed a [`Sequence`] value which can be used to add events and other
/// patterns to this sequence. See the methods on [`Sequence`] for more details.
///
/// The argument passed to the given closure is conventally called `s` because it is repeated so
/// often in pattern definitions.
pub fn sequence<M>(f: impl FnOnce(&mut Sequence<M>)) -> Pattern<M> {
    let mut seq = Sequence(Patterns::new());
    f(&mut seq);
    let Sequence(Patterns(patterns)) = seq;
    Pattern(PatternInner::Sequence(patterns))
}

/// A builder for sequential patterns.
///
/// Passed to the closure given to [`sequence`] and [`Parallel::sequence`];
#[derive(Debug)]
pub struct Sequence<M>(Patterns<M>);

impl<M> Sequence<M> {
    /// Add an event to this sequence.
    ///
    /// # Examples
    ///
    /// ```
    /// use sorceress::pattern::{sequence, Event, Pattern};
    ///
    /// let pattern: Pattern<String> = sequence(|s| {
    ///     s.play(1.0, "B");
    ///     s.play(1.0, "A");
    ///     s.play(1.0, "C");
    /// });
    ///
    /// let mut events = pattern.into_iter();
    ///
    /// assert_eq!(Some(Event::new(1.0, "B")), events.next());
    /// assert_eq!(Some(Event::new(1.0, "A")), events.next());
    /// assert_eq!(Some(Event::new(1.0, "C")), events.next());
    /// assert_eq!(None, events.next());
    /// ```
    pub fn play(&mut self, delta: f64, event: impl Into<M>) {
        self.0.play(delta, event.into())
    }

    /// Add a rest to the sequence.
    ///
    /// Rests are essentially events without any event data. Rests signal to schedulers that no
    /// actions should be taken at this time but the logical time of the sequence should move
    /// forward. Rests are required to delay the first event in a sequence.
    pub fn rest(&mut self, delta: f64) {
        self.0.rest(delta)
    }

    /// Extend this sequence with another pattern.
    ///
    /// # Examples
    ///
    /// ```
    /// use sorceress::pattern::{sequence, Event, Pattern};
    ///
    /// let section1: Pattern<String> = sequence(|p| {
    ///     p.play(1.0, "hello");
    ///     p.play(1.0, "there");
    /// });
    /// let section2: Pattern<String> = sequence(|s| {
    ///     s.play(1.0, "dear");
    ///     s.play(1.0, "friend");
    /// });
    /// let pattern: Pattern<String> = sequence(|s| {
    ///     s.embed(section1);
    ///     s.embed(section2);
    /// });
    ///
    /// let mut events = pattern.into_iter();
    ///
    /// assert_eq!(Some(Event::new(1.0, "hello")), events.next());
    /// assert_eq!(Some(Event::new(1.0, "there")), events.next());
    /// assert_eq!(Some(Event::new(1.0, "dear")), events.next());
    /// assert_eq!(Some(Event::new(1.0, "friend")), events.next());
    /// assert_eq!(None, events.next());
    /// ```
    pub fn embed(&mut self, pattern: Pattern<M>) {
        self.0.embed(pattern)
    }

    /// Create a parallel pattern and embed it into this sequence.
    ///
    /// This method is simply shorthand for `self.embed(parallel(f))`.
    pub fn parallel(&mut self, f: impl FnOnce(&mut Parallel<M>)) {
        self.0.embed(parallel(f))
    }
}

/// Play multiple sequences at the same time.
///
/// The given closure will be passed a [`Parallel`] value which can be used to add events and other
/// patterns to this collection. See the methods on [`Parallel`] for more details.
///
/// The argument passed to the given closure is conventally called `p` because it is repeated so
/// often in pattern definitions.
pub fn parallel<M>(f: impl FnOnce(&mut Parallel<M>)) -> Pattern<M> {
    let mut seq = Parallel(Patterns::new());
    f(&mut seq);
    let Parallel(Patterns(patterns)) = seq;
    Pattern(PatternInner::Parallel(patterns))
}

/// A builder for parallel patterns.
///
/// Passed to the closure given to [`parallel`] and [`Sequence::parallel`];
#[derive(Debug)]
pub struct Parallel<M>(Patterns<M>);

impl<M> Parallel<M> {
    /// Play an event in parallel with other sequences and events in this parallel pattern.
    ///
    /// # Examples
    ///
    /// This can be used to express [musical chords] in a pattern.
    ///
    /// ```
    /// use sorceress::pattern::{parallel, Event, Pattern};
    ///
    /// let pattern: Pattern<String> = parallel(|p| {
    ///     p.play(1.0, "F");
    ///     p.play(1.0, "A");
    ///     p.play(1.0, "C");
    /// });
    ///
    /// let mut events = pattern.into_iter();
    ///
    /// assert_eq!(Some(Event::new(0.0, "F")), events.next());
    /// assert_eq!(Some(Event::new(0.0, "A")), events.next());
    /// assert_eq!(Some(Event::new(1.0, "C")), events.next());
    /// assert_eq!(None, events.next());
    /// ```
    ///
    /// [musical chords]: https://en.wikipedia.org/wiki/Chord_(music)
    pub fn play(&mut self, delta: f64, event: impl Into<M>) {
        self.0.play(delta, event.into())
    }

    /// Play another [`Pattern`] in parallel.
    ///
    /// # Examples
    ///
    /// ```
    /// use sorceress::pattern::{parallel, sequence, Event, Pattern};
    ///
    /// let chord: Pattern<String> = parallel(|p| {
    ///     p.play(4.0, "F4");
    ///     p.play(4.0, "A5");
    ///     p.play(4.0, "C5");
    /// });
    /// let bass: Pattern<String> = sequence(|s| {
    ///     s.play(1.0, "E2");
    ///     s.play(1.0, "C2");
    ///     s.play(2.0, "F2");
    /// });
    /// let pattern: Pattern<String> = parallel(|p| {
    ///     p.embed(chord);
    ///     p.embed(bass);
    /// });
    ///
    /// let mut events = pattern.into_iter();
    ///
    /// assert_eq!(Some(Event::new(0.0, "F4")), events.next());
    /// assert_eq!(Some(Event::new(0.0, "A5")), events.next());
    /// assert_eq!(Some(Event::new(0.0, "C5")), events.next());
    /// assert_eq!(Some(Event::new(1.0, "E2")), events.next());
    /// assert_eq!(Some(Event::new(1.0, "C2")), events.next());
    /// assert_eq!(Some(Event::new(2.0, "F2")), events.next());
    /// assert_eq!(None, events.next());
    /// ```
    pub fn embed(&mut self, pattern: Pattern<M>) {
        self.0.embed(pattern)
    }

    /// Create another sequence and play it in parallel.
    ///
    /// This method is simply shorthand for `self.embed(sequence(f))`.
    pub fn sequence(&mut self, f: impl FnOnce(&mut Sequence<M>)) {
        self.0.embed(sequence(f));
    }
}

#[derive(Debug)]
struct Patterns<M>(Vec<Pattern<M>>);

impl<M> Patterns<M> {
    fn new() -> Patterns<M> {
        Patterns(Vec::new())
    }

    fn play(&mut self, delta: f64, event: M) {
        self.0.push(Pattern(PatternInner::Event(Event {
            delta,
            event: EventOrRest::Event(event),
        })));
    }

    fn rest(&mut self, delta: f64) {
        self.0.push(Pattern(PatternInner::Event(Event {
            delta,
            event: EventOrRest::Rest,
        })));
    }

    fn embed(&mut self, pattern: Pattern<M>) {
        self.0.push(pattern);
    }
}

/// An iterator that transforms a pattern into a flat sequence of events.
///
/// Returned by the `into_iter` method on [`Pattern`].
#[must_use]
#[derive(Debug)]
pub struct IntoIter<M>(IntoIterInner<M>);

#[derive(Debug)]
enum IntoIterInner<M> {
    Event(iter::Once<Event<M>>),
    Parallel(ParallelIter<M>),
    Sequence(Box<SequenceIntoIter<M>>),
}

type SequenceIntoIter<M> =
    iter::FlatMap<std::vec::IntoIter<Pattern<M>>, IntoIter<M>, fn(Pattern<M>) -> IntoIter<M>>;

impl<M> IntoIterator for Pattern<M> {
    type Item = Event<M>;
    type IntoIter = IntoIter<M>;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter(match self.0 {
            PatternInner::Event(event) => IntoIterInner::Event(iter::once(event)),
            PatternInner::Parallel(patterns) => {
                IntoIterInner::Parallel(ParallelIter::new(patterns))
            }
            PatternInner::Sequence(patterns) => {
                IntoIterInner::Sequence(Box::new(patterns.into_iter().flat_map(Pattern::into_iter)))
            }
        })
    }
}

impl<M> Iterator for IntoIter<M> {
    type Item = Event<M>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.0 {
            IntoIterInner::Event(ref mut iter) => iter.next(),
            IntoIterInner::Parallel(ref mut iter) => iter.next(),
            IntoIterInner::Sequence(ref mut iter) => iter.next(),
        }
    }
}

#[must_use]
#[derive(Debug)]
struct ParallelIter<M> {
    merged: Peekable<Merged<M>>,
}

impl<M> ParallelIter<M> {
    fn new(patterns: Vec<Pattern<M>>) -> Self {
        Self {
            merged: Merged::new(patterns).peekable(),
        }
    }
}

impl<M> Iterator for ParallelIter<M> {
    type Item = Event<M>;

    fn next(&mut self) -> Option<Self::Item> {
        let (position, mut event) = self.merged.next()?;
        if let Some((next_position, _)) = self.merged.peek() {
            event.delta = next_position - position;
        }
        Some(event)
    }
}

#[must_use]
#[derive(Debug)]
struct Merged<M> {
    iters: Vec<Peekable<Position<M>>>,
}

impl<M> Merged<M> {
    fn new(patterns: Vec<Pattern<M>>) -> Self {
        Self {
            iters: patterns
                .into_iter()
                .map(|pattern| Position::new(pattern.into_iter()).peekable())
                .collect(),
        }
    }
}

impl<M> Iterator for Merged<M> {
    type Item = (f64, Event<M>);

    fn next(&mut self) -> Option<Self::Item> {
        self.iters
            .iter_mut()
            .flat_map(|iter| {
                let (position, _) = iter.peek()?;
                Some((*position, iter))
            })
            .min_by(|(position1, _), (position2, _)| position1.partial_cmp(position2).unwrap())
            .and_then(|(_, iter)| iter.next())
    }
}

#[must_use]
#[derive(Debug)]
struct Position<M> {
    position: f64,
    iter: IntoIter<M>,
}

impl<M> Position<M> {
    fn new(iter: IntoIter<M>) -> Self {
        Self {
            position: 0.0,
            iter,
        }
    }
}

impl<M> Iterator for Position<M> {
    type Item = (f64, Event<M>);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|event| {
            let position = self.position;
            self.position += event.delta;
            (position, event)
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parallel_test_empty_seq() {
        assert!(stream_pattern(sequence(|_| {})).is_empty());
    }

    #[test]
    fn parallel_test_left_identity() {
        let a: Pattern<i32> = sequence(|s| {
            s.play(1.0, 0);
            s.play(1.0, 0);
        });
        let b: Pattern<i32> = sequence(|_| {});
        assert_eq!(
            stream_pattern(a.clone()),
            stream_pattern(parallel(|p| {
                p.embed(a);
                p.embed(b);
            })),
        );
    }

    #[test]
    fn parallel_test_right_identity() {
        let a: Pattern<i32> = sequence(|_| {});
        let b: Pattern<i32> = sequence(|s| {
            s.play(1.0, 0);
            s.play(1.0, 0);
        });
        assert_eq!(
            stream_pattern(b.clone()),
            stream_pattern(parallel(|p| {
                p.embed(a);
                p.embed(b);
            })),
        );
    }

    #[test]
    fn parallel_test_delta_adjustment() {
        let a: Pattern<i32> = sequence(|s| {
            s.play(1.0, 1);
            s.play(1.0, 2);
        });
        let b: Pattern<i32> = sequence(|s| {
            s.play(1.0, 3);
            s.play(1.0, 4);
        });
        assert_eq!(
            vec![
                Event::new(0.0, 1),
                Event::new(1.0, 3),
                Event::new(0.0, 2),
                Event::new(1.0, 4)
            ],
            stream_pattern(parallel(|p| {
                p.embed(a);
                p.embed(b);
            })),
        );
    }

    #[test]
    fn parallel_test_uneven_lengths() {
        let a: Pattern<i32> = sequence(|s| s.play(1.0, 1));
        let b: Pattern<i32> = sequence(|s| {
            s.play(1.0, 2);
            s.play(1.0, 3);
        });
        assert_eq!(
            vec![Event::new(0.0, 1), Event::new(1.0, 2), Event::new(1.0, 3),],
            stream_pattern(parallel(|p| {
                p.embed(a);
                p.embed(b)
            })),
        );
    }

    fn stream_pattern(pattern: Pattern<i32>) -> Vec<Event<i32>> {
        pattern.into_iter().collect()
    }
}
