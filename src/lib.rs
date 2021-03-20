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

#![forbid(unsafe_code)]

//! A Rust environment for sound synthesis and algorithmic composition, powered by
//! [SuperCollider](https://supercollider.github.io/).
//!
//! # Introduction
//!
//! This crate provides Rust bindings to the SuperCollider audio synthesis and algorithmic
//! composition platform.
//!
//! There is no better overview of SuperCollider than the summary from its home page:
//!
//! > "SuperCollider features three major components:
//! >
//! > * **scsynth**, a real-time audio server, forms the core of the platform. It features 400+
//! >   unit generators (“UGens”) for analysis, synthesis, and processing. Its granularity allows
//! >   the fluid combination of many known and unknown audio techniques, moving between additive
//! >   and subtractive synthesis, FM, granular synthesis, FFT, and physical modeling. You can
//! >   write your own UGens in C++, and users have already contributed several hundred more to the
//! >   sc3-plugins repository.
//! >
//! > * **sclang**, an interpreted programming language. It is focused on sound, but not limited to
//! >   any specific domain. sclang controls scsynth via Open Sound Control. You can use it for
//! >   algorithmic composition and sequencing, finding new sound synthesis methods, connecting
//! >   your app to external hardware including MIDI controllers, network music, writing GUIs and
//! >   visual displays, or for your daily programming experiments. It has a stock of
//! >   user-contributed extensions called Quarks.
//! >
//! > * **scide** is an editor for sclang with an integrated help system."
//!
//! Sorceress is a Rust alternative to **sclang**. It provides features for interacting with
//! SuperCollider similar to those found in sclang. It currently contains:
//!
//! * [`synthdef`](crate::synthdef) - Core types for designing new synthesizers by creating "synth
//!   definitions". Synth definitions are graphs of connected unit generators.
//!
//! * [`ugen`](crate::ugen) - A library of unit generators or "UGens". **scsynth** provides
//!   hundreds of unit generators and sorceress aims to provide interfaces for all of them some
//!   day, but its going to take a while. Please raise an issue on the source repository if there
//!   are UGens that you'd like to see implemented so I know what to prioritize.
//!
//! * [`pattern`](crate::pattern) - A module for defining patterns. Patterns are collections of
//!   events with timing information. Patterns are created by combining collections of events in
//!   sequence or in parallel. Patterns are the primary way of composing music with sorceress. The
//!   [`scheduler`](crate::scheduler) module can be used in combination with the
//!   [`pattern::player`](crate::pattern::player) submodule to play patterns.
//!
//! * [`scheduler`](crate::scheduler) - A module for running code on a recurring schedule. The
//!   scheduler is responsible handling the timing details of executing patterns.
//!
//! * [`server`](crate::server) - A [`Server`](server::Server) type that manages communication
//!   with scsynth and Rust definitions of all of the OSC commands understood by the server.
//!   Similar to the library of UGens, that are many commands to implement, around 65, so please
//!   create an issue if you would like to see certain command implemented sooner.
//!
//! # Examples
//!
//! This example plays the opening melody Stravinsky's The Rite of Spring.
//!
//! ```no_run
//! use sorceress::{
//!     pattern::{
//!         player::{Player, Tempo},
//!         sequence, Pattern,
//!     },
//!     scheduler::Scheduler,
//!     server::{self, Bundle, Control, Server},
//!     synthdef::{encoder::encode_synth_defs, Input, SynthDef},
//!     ugen,
//! };
//!
//! type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;
//!
//! fn main() -> Result<()> {
//!     // Create a new SuperCollider server client. A SuperCollider server must be started
//!     // outside of this program and be configured to listen on this UDP port.
//!     let server = Server::connect("127.0.0.1:57110")?;
//!
//!     // Create a synth definition.
//!     let sine_wave = {
//!         // Create and name the synth definition. We will use the name given here to refer
//!         // to this synth definition in all server commands.
//!         SynthDef::new("sine_wave", |params| {
//!             // Parameters allow synths created by this definition to be configured with
//!             // different values when they are created, or be changed while they are playing.
//!             let freq = params.named("freq", 0.0);
//!
//!             // Creates a sine wave that oscillates between -7 and 7, 5 times per second.
//!             let vibrato = ugen::SinOsc::ar().freq(5).mul(7);
//!
//!             // Sends the audio signal to bus 0, which represents your speakers.
//!             ugen::OffsetOut::ar()
//!                 .bus(0)
//!                 // Pan2 turns a mono signal into a stereo signal.
//!                 .channels(
//!                     ugen::Pan2::ar()
//!                         // SuperCollider lets us plug UGens into the inputs of other UGens
//!                         // to create complex signal processing graphs. Here we are using
//!                         // the vibrato SinOsc UGen defined above to slightly oscillate the
//!                         // pitch of the sine wav we will actually hear.
//!                         .input(freq.mul(vibrato)),
//!                 )
//!         })
//!     };
//!
//!     // Send the synth definition to the SuperCollider server so that we can make synths
//!     // using it.
//!     let encoded_synthdef = encode_synth_defs(vec![sine_wave]);
//!     server.send_sync(server::SynthDefRecv::new(&encoded_synthdef))?;
//!
//!     // Play a sequence of notes. The first number is the duration in beats and the second
//!     // number is the midi note number.
//!     let pattern: Pattern<Event> = sequence(|s| {
//!         s.play(1.2, Event::StartNote(72.0));
//!         s.play(1.5, Event::SetPitch(71.0));
//!         s.play(1.5, Event::SetPitch(72.0));
//!         s.play(3.0, Event::SetPitch(71.0));
//!         s.play(3.0, Event::SetPitch(67.0));
//!         s.play(3.0, Event::SetPitch(64.0));
//!         s.play(3.0, Event::SetPitch(71.0));
//!         s.play(6.0, Event::SetPitch(69.0));
//!         s.play(4.0, Event::SetPitch(72.0));
//!         s.play(2.0, Event::SetPitch(71.0));
//!         s.play(2.0, Event::SetPitch(72.0));
//!         s.play(4.0, Event::SetPitch(71.0));
//!         s.play(4.0, Event::SetPitch(67.0));
//!         s.play(4.0, Event::SetPitch(64.0));
//!         s.play(4.0, Event::SetPitch(71.0));
//!         s.play(6.0, Event::SetPitch(69.0));
//!     });
//!
//!     let event_handler = |time, event| {
//!         // We launch a single synth and set its pitch multiple times to create a melody. Synth
//!         // IDs are numbers that let us to refer to synths after we crate them.
//!         let synth_id: i32 = 1000;
//!
//!         let result = match event {
//!             Event::StartNote(midi_note) => {
//!                 // Create a new synth from the synth definition we registered earlier.
//!                 let command = server::SynthNew::new("sine_wave", 1)
//!                     // Convert the midi note into hertz. The "freq" string refers to the string
//!                     // given to the `Parameter` in the synth definition above.
//!                     .controls(vec![Control::new("freq", midi_to_hz(midi_note))])
//!                     // Explicitly set the synth ID so that we can control it later.
//!                     .synth_id(synth_id);
//!
//!                 server
//!                     // Ask SuperCollider to schedule this event at exactly this time.
//!                     .send(Bundle::new(time, vec![command]))
//!             }
//!             Event::SetPitch(midi_note) => {
//!                 // Convert the midi note into hertz.
//!                 let controls = vec![Control::new("freq", midi_to_hz(midi_note))];
//!
//!                 // Changes the pitch of the actively playing synth.
//!                 let command = server::NodeSet::new(synth_id, controls);
//!                 server.send(Bundle::new(time, vec![command]))
//!             }
//!         };
//!         if let Err(err) = result {
//!             println!("error: sending event to server: {}", err);
//!         };
//!     };
//!
//!     // Play the pattern using the scheduler.
//!     Scheduler::new().run(Player::new(pattern, event_handler).tempo(Tempo::from_bpm(60.0)))?;
//!
//!     // Stop all sounds on the server.
//!     server.reset()?;
//!
//!     Ok(())
//! }
//!
//! // Our event type.
//! enum Event {
//!     StartNote(f32),
//!     SetPitch(f32),
//! }
//!
//! // Convert a midi note number into a frequency in hertz.
//! fn midi_to_hz(note: f32) -> f32 {
//!     let exp = (note - 69.0) / 12.0;
//!     440.0 * 2f32.powf(exp)
//! }
//! ```
//!
//! # Learning SuperCollider
//!
//! SuperCollider is a powerful platform and there are many concepts to learn on the path to
//! proficiency. Currently, the documentation in this crate is not thorough enough to let people to
//! get started using SuperCollider for the first time with sorceress. However, the Supercollider
//! project provides [excellent documentation](https://doc.sccode.org/Help.html) and much of it
//! carries over to using sorceress. If this is your first time learning about SuperCollider I
//! recommend going through the following items before diving into sorceress:
//!
//! * Read the [Client vs Server](https://doc.sccode.org/Guides/ClientVsServer.html) documentation
//!   to understand the different roles that SuperCollider and sorceress play.  Remember that
//!   sorceress is a replacement for the sclang, SuperCollider's interpreted object-oriented
//!   language.
//!
//! * Go through the [Getting Started With SuperCollider] tutorial series. This will teach you the
//!   all of the fundementals of SuperCollider and how to boot **scsynth** on your computer.
//!
//! * Poke around the [Server Command Reference] to understand all of the OSC commands that
//!   SuperCollider accepts. The [server](crate::server) module does not yet contain all of these
//!   commands, so this document is useful for seeing what is available and what is currently
//!   missing.
//!
//! # Planned Features
//!
//! * Declarative resource management - sclang allows arbitrary sections of code in a source code
//!   file to be executed in any order using **scide**. This makes it very easy to run setup code
//!   that creates server-side resources such as buffers and synthdefs once before executing the
//!   code that creates synths and makes sounds. Instead of replicating this in Rust through
//!   condition execution of parts of code, we can create a declarative interface for managing
//!   server side resources. Code using this crate will *declare* what resources it needs, and
//!   resource management code provided by this crate will find the difference between what is
//!   currently provisioned and what is desired and automatically reconcile the two.
//!
//! * A live reloading development workflow - Dynamic interpreted languages like sclang provide an
//!   excellent environment for creative coding. The nature of how code is parsed and executed in
//!   those languages makes it easy to replace portions of code on the fly, at runtime, which is
//!   excellent for iterating on musical pieces. Rust presents a unique challenge in this area, as
//!   code cannot as easily be replaced during runtime. A similar workflow can still be achieved in
//!   Rust using a preemptible scheduler that can be paused and resumed when a new version of the
//!   code successfully compiles. If we add compilation triggered by filesystem notifications, we
//!   can create a streamlined creative workflow for creating music with Rust.
//!
//! [Server Command Reference]: https://doc.sccode.org/Reference/Server-Command-Reference.html
//! [Getting Started With SuperCollider]: https://doc.sccode.org/Tutorials/Getting-Started/01-Introductory-Remarks.html
//! [`Pseq`]: https://doc.sccode.org/Classes/Pseq.html
//! [`Ppar`]: https://doc.sccode.org/Classes/Ppar.html
//! [`Pn`]: https://doc.sccode.org/Classes/Pn.html
//! [`TempoClock`]: https://doc.sccode.org/Classes/TempoClock.html
//! [Sequencing with Patterns]: https://doc.sccode.org/Tutorials/Getting-Started/16-Sequencing-with-Patterns.html

pub mod pattern;
pub mod scheduler;
pub mod server;
pub mod synthdef;
pub mod ugen;
mod vectree;
