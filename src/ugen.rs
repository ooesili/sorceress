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

//! The library of UGens.
//!
//! UGens, short for *unit generators*, are primitives offered by SuperCollider that generate and
//! process audio and control signals. UGens are used in [synth definitions](crate::synthdef) as
//! nodes in the signal graphs that eventually produce sound.

pub mod envelope;

use crate::synthdef::{DoneAction, IntoValue, Rate, Scalar, UGenSpec, UgenInput, Value};
use crate::vectree::VecTree;
use envelope::Env;

macro_rules! ugen_rate_constructor {
    ( ar ) => {
        /// Create a UGen that calculates samples at audio rate.
        pub fn ar() -> Self {
            Self::new_with_rate(Rate::Audio)
        }
    };
    ( ir ) => {
        /// Create a UGen that calculates samples at initialization only.
        pub fn ir() -> Self {
            Self::new_with_rate(Rate::Scalar)
        }
    };
    ( kr ) => {
        /// Create a UGen that calculates samples at control rate.
        pub fn kr() -> Self {
            Self::new_with_rate(Rate::Control)
        }
    };
}

macro_rules! ugen_set_value {
    ( $spec:ident, $ugen:ident, multi, $input:ident ) => {
        $spec.inputs.push(UgenInput::Multi($ugen.$input));
    };
    ( $spec:ident, $ugen:ident, $t:ty, $input:ident ) => {
        $spec.inputs.push(UgenInput::Simple($ugen.$input));
    };
}

macro_rules! ugen_set_option {
    ( $spec:ident, $ugen:ident, num_outputs, $count:expr) => {
        $spec.outputs = vec![$ugen.rate; $count];
    };
    ( $spec:ident, $ugen:ident, special_index, $index:expr) => {
        $spec.special_index = $index;
    };
}

macro_rules! ugen {
    ( $(#[$struct_meta:meta])*
      $name:ident[ $( $rate:ident ),* ] {
        inputs: $inputs:tt
    } ) => {
        ugen! {
            $(#[$struct_meta])*
            $name[ $( $rate ),* ] {
                inputs: $inputs,
                options: {}
            }
        }
    };
    ( $(#[$struct_meta:meta])*
      $name:ident[ $( $rate:ident ),* ] {
        inputs: { $(
            $(#[$input_meta:meta])*
            $input:ident: $type:tt = $default_value:expr
        ),* },
        options: { $( $option:ident: $option_value:tt ),* }
    } ) => {
        $(#[$struct_meta])*
        #[derive(Debug, PartialEq)]
        pub struct $name {
            rate: Rate,
            $( $input: Value, )*
        }

        impl $name {
            fn new_with_rate(rate: Rate) -> Self {
                Self{
                    rate,
                    $( $input: $default_value.into(), )*
                }
            }

            $( ugen_rate_constructor!($rate); )*

            $(
                $(#[$input_meta])*
                pub fn $input(mut self, value: impl Into<Value>) -> Self {
                    self.$input = value.into();
                    self
                }
            )*
        }

        impl From<$name> for Value {
            fn from(mut _ugen: $name) -> Self {
                let mut _spec = UGenSpec{
                    name: stringify!($name).to_owned(),
                    rate: _ugen.rate,
                    special_index: 0,
                    inputs: vec![],
                    outputs: vec![_ugen.rate],
                };

                $( ugen_set_value!(_spec, _ugen, $type, $input); )*
                $( ugen_set_option!(_spec, _ugen, $option, $option_value); )*
                _spec.into()
            }
        }
    };
}

ugen! {
    /// Two channel equal power pan.
    ///
    /// `Pan2` takes the square root of the linear scaling factor going from 1 (left or right) to
    /// `sqrt(0.5)` (~=0.707) in the center, which is about 3dB reduction. With linear panning
    /// [`LinPan2`] the signal is lowered as it approaches center using a straight line from 1
    /// (left or right) to 0.5 (center) for a 6dB reduction in the middle. A problem inherent to
    /// linear panning is that the perceived volume of the signal drops in the middle. `Pan2`
    /// solves this.
    Pan2[ar, kr] {
        inputs: {
            /// The input signal.
            input: f32 = 0,

            /// Pan position, -1 is left, +1 is right.
            pos: f32 = 0,

            /// A control rate level input.
            level: f32 = 1
        },
        options: {
            num_outputs: 2
        }
    }
}

ugen! {
    /// A stereo signal balancer.
    ///
    /// Equal power panning balances two channels. By panning from left (pos = -1) to right (pos =
    /// 1) you are decrementing the level of the left channel from 1 to 0 taking the square root of
    /// the linear scaling factor, while at the same time incrementing the level of the right
    /// channel from 0 to 1 using the same curve. In the center position (pos = 0) this results in
    /// a level for both channels of the square root of one half (~=0.707 or -3dB). The output of
    /// `Balance2` remains a stereo signal.
    Balance2[ar, kr] {
        inputs: {
            /// Channel 1 of input stereo signal.
            left: f32 = 0,
            /// Channel 2 of input stereo signal.
            right: f32 = 0,
            /// Pan position, `-1` is left, `+1` is right.
            pos: f32 = 0,
            /// A control rate level input.
            level: f32 = 1
        },
        options: {
            num_outputs: 2
        }
    }
}

/// Bring signals and floats into the UGen graph of a SynthDef.
///
/// A `Control` is a UGen that can be set and routed externally to interact with a running Synth.
/// Typically, Controls are created from parameters in synth definitions.
///
/// Generally you do not create Controls yourself. (See Arrays example below).
///
/// The rate may be either `kr` (continuous control rate signal) or `ir` (a static value, set at
/// the time the synth starts up, and subsequently unchangeable). For `ar`, see [`AudioControl::ar`].
///
/// Synth definitions create these automatically when encoding the UGen graph. They are created for
/// you, you use them, and you don't really need to worry about them if you don't want to.
//
// For a more concise combination of name, default value and lag, see [NamedControl]
//
// TODO: port Control examples from SuperCollider docs
pub struct Control {
    rate: Rate,
    values: Vec<Value>,
}

impl Control {
    /// Create a UGen that calculates samples at control rate.
    pub fn kr<T: Into<Value>>(values: impl IntoIterator<Item = T>) -> Control {
        Control {
            rate: Rate::Control,
            values: values.into_iter().map(T::into).collect(),
        }
    }

    /// Create a UGen that calculates samples at initialization only.
    pub fn ir<T: Into<Value>>(values: impl IntoIterator<Item = T>) -> Control {
        Control {
            rate: Rate::Scalar,
            values: values.into_iter().map(T::into).collect(),
        }
    }
}

impl From<Control> for Value {
    fn from(ugen: Control) -> Value {
        UGenSpec {
            name: "Control".to_owned(),
            rate: ugen.rate,
            special_index: 0,
            inputs: ugen.values.into_iter().map(UgenInput::Simple).collect(),
            outputs: vec![Rate::Audio],
        }
        .into()
    }
}

// ugen! {
//     Control[ar, kr] {
//         inputs: {}
//     }
// }

// TODO: add `.mul` and `.add` convenience functions

ugen! {
    /// Band limited sawtooth wave generator.
    Saw[ar, kr] {
        inputs: {
            /// Frequency in hertz.
            freq: f32 = 440
        }
    }
}

ugen! {
    /// A non-band-limited sawtooth oscillator.
    ///
    /// Output ranges from -1 to +1.
    LFSaw[ar, kr] {
        inputs: {
            /// Frequency in hertz.
            freq: f32 = 440,

            /// Initial phase offset. For efficiency reasons this is a value ranging from 0 to 2.
            initial_phase: f32 = 0.0
        }
    }
}

ugen! {
    /// Comb delay line with no interpolation.
    ///
    /// See also [`CombLwhich`] uses linear interpolation, and [`CombC`] which uses cubic
    /// interpolation. Cubic and linear interpolation are more computationally expensive, but more
    /// accurate.
    ///
    /// This UGen will create aliasing artifacts if you modulate the delay time, which is also
    /// quantized to the nearest sample period. If these are undesirable properties, use `CombL` or
    /// `CombC`. But if your delay time is fixed and sub-sample accuracy is not needed, this is the
    /// most CPU-efficient choice with no loss in quality.
    ///
    /// The feedback coefficient is given by a mathmatical function that looks like this, where 0.001 is -60 dBFS:
    ///
    /// ```
    /// fn feedback(delay: f32, decay: f32) -> f32 {
    ///     let sign = if decay < 0.0 {
    ///         -1.0
    ///     } else if decay > 0.0 {
    ///         1.0
    ///     } else {
    ///         0.0
    ///     };
    ///     0.001f32.powf((delay / decay.abs()) * sign)
    /// }
    /// ```

    CombN[ar, kr] {
        inputs: {
            /// The input signal.
            input: f32 = 0.0,

            /// The maximum delay time in seconds. Used to initialize the delay buffer size.
            max_delay_time: f32 = 0.2,

            /// Delay time in seconds.
            delay_time: f32 = 0.2,
            /// Time for the echoes to decay by 60 decibels. If this time is negative, then the
            /// feedback coefficient will be negative, thus emphasizing only odd harmonics at an
            /// octave lower.
            ///
            /// Large decay times are sensitive to DC bias, so use a [`LeakDC`] if this is an
            /// issue.
            ///
            /// Infinite decay times are permitted. A decay time of [`f32::INFINITY`] leads to a
            /// feedback coefficient of 1, and a decay time of `-f32::INFINITY` leads to a feedback
            /// coefficient of -1.
            decay_time: f32 = 1.0
        }
    }
}

ugen! {
    /// A timed trigger.
    ///
    /// When a nonpositive to positive transition occurs at the input, `Trig` outputs the level of
    /// the triggering input for the specified duration, otherwise it outputs zero.
    Trig[ar, kr] {
        inputs: {
            /// The trigger, which can be any signal. A trigger happens when the signal changes
            /// from non-positive to positive.
            input: f32 = 0,

            /// Duration of the trigger output.
            duration: f32 = 0.1
        }
    }
}

ugen! {
    /// Impulse oscillator.
    ///
    /// Outputs non-bandlimited single sample impulses. An `Impulse` with frequency `0` returns a
    /// single impulse.
    Impulse[ar, kr] {
        inputs: {
            /// The frequency in hertz.
            freq: i32 = 440,

            /// Phase offset in cycles, from `0` to `1`.
            phase: f32 = 0
        }
    }
}

ugen! {
    /// Interpolating sine wavetable oscillator.
    ///
    /// Generates a sine wave. Uses a wavetable lookup oscillator with linear interpolation.
    /// Frequency and phase modulation are provided for audio-rate modulation. Technically,
    /// `SinOsc` uses the same implementation as [`Osc`] except that its table is fixed to be a
    /// sine wave made of 8192 samples.
    ///
    /// #  Other Sinewave Oscillators
    ///
    /// *  [`FSinOsc`] – fast sinewave oscillator
    /// *  [`SinOscFB`] – sinewave with phase feedback
    /// *  [`PMOsc`] – phase modulation sine oscillator
    /// *  [`Klang`] – bank of sinewave oscillators
    /// *  [`DynKlang`] – modulable bank of sinewave oscillators
    SinOsc[ar, kr] {
        inputs: {
            /// Frequency in hertz. Sampled at audio-rate.
            freq: f32 = 440,

            /// Phase in radians. Sampled at audio-rate.
            ///
            /// **Note**: phase values should be within the range +-8pi.
            // TODO: add `.mod` operator
            // If your phase values are larger then simply use .mod(2pi) to wrap them.
            phase: f32 = 0
        }
    }
}

ugen! {
    /// Write a signal to a bus.
    //
    // TODO: add a bus type
    //
    // Note that using the Bus class to allocate a multichannel bus simply reserves a series of
    // adjacent bus indices with the Server object's bus allocators. abus.index simply returns the
    // first of those indices. When using a Bus with an In or Out UGen there is nothing to stop
    // you from reading to or writing from a larger range, or from hardcoding to a bus that has
    // been allocated. You are responsible for making sure that the number of channels match and
    // that there are no conflicts.
    ///
    /// **Note**: Out is subject to control rate jitter. Where sample accurate output is needed,
    /// use [`OffsetOut`].
    ///
    /// See the [Server Architecture] and [Bus] SuperCollider helpfiles for more information on
    /// buses and how they are used.
    ///
    /// [Server Architecture]: https://doc.sccode.org/Reference/Server-Architecture.html
    /// [Bus]: https://doc.sccode.org/Classes/Bus.html
    Out[ar, kr] {
        inputs: {
            /// The index of the bus to write out to. The lowest numbers are written to the audio
            /// hardware.
            bus: f32 = 0,
            /// A list of channels or single output to write out. You cannot change the size of
            /// this once a synth definition has been built.
            channels: multi = 0
        },
        options: {
            num_outputs: 0
        }
    }
}

ugen! {
    /// Write a signal to a bus with sample accurate timing.
    ///
    /// Output signal to a bus, the sample offset within the bus is kept exactly; i.e. if the synth
    /// is scheduled to be started part way through a control cycle, `OffsetOut` will maintain the
    /// correct offset by buffering the output and delaying it until the exact time that the synth
    /// was scheduled for.
    ///
    /// **Note**: If you have an input to the synth, it will be coming in and its normal time, then
    /// mixed in your synth, and then delayed with the output. So you shouldn't use OffsetOut for
    /// effects or gating.
    ///
    /// See the [Server Architecture] and [Bus] SuperCollider helpfiles for more information on
    /// buses and how they are used.
    ///
    /// [Server Architecture]: https://doc.sccode.org/Reference/Server-Architecture.html
    /// [Bus]: https://doc.sccode.org/Classes/Bus.html
    OffsetOut[ar, kr] {
        inputs: {
            /// The index of the bus to write out to. The lowest numbers are written to the audio
            /// hardware.
            bus: f32 = 0,
            /// A list of channels or single output to write out. You cannot change the size of
            /// this once a synth definition has been built.
            channels: multi = 0
        },
        options: {
            num_outputs: 0
        }
    }
}

ugen! {
    /// Record to a soundfile to disk. Uses a Buffer.
    ///
    /// See [`RecordBuf`] for recording into a buffer in memory.
    ///
    /// # Disk recording procedure:
    ///
    /// Recording to disk involves several steps, which should be taken in the right order. To
    /// record buses using DiskOut, make sure to do the following:
    ///
    /// 1. Define a DiskOut SynthDef, as shown in the example below.
    /// 2. Allocate a buffer using [`BufferAllocate`](crate::server::BufferAllocate) for recording.
    ///    * The buffer size should be a power of two.
    ///    * A duration of at least one second is recommended.
    ///    * Do not allocate the buffer inside the SynthDef.
    ///
    /// 3. Specify the file path and recording format using
    ///    [`BufferWrite`](crate::server::BufferWrite) with the
    ///    [`leave_file_open()`](crate::server::BufferWrite::leave_file_open) flag enabled.
    ///    This is the only way to set the file path and recording format.
    /// 4. Create a synth node to run the DiskOut UGen with [`SynthNew`](crate::server::SynthNew).
    /// 5. When recording is finished, stop the `DiskOut` synth.
    /// 6. Close the buffer with [`BufferClose`](crate::server::BufferClose). This step updates the
    ///    recorded file's audio header. Without it, the file will be unusable.
    /// 7. Free the buffer with [`BufferFree`](crate::server::BufferFree).
    ///
    /// These steps are illustrated in the Examples section.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use sorceress::server::{self, Result, Server};
    /// use std::{iter, thread, time::Duration};
    ///
    /// # fn main() -> Result<()> {
    /// // Connect to a running SuperCollider server.
    /// let server = Server::connect("127.0.0.1:57110")?;
    ///
    // TODO: create and load synth definitions here
    //
    /// // Start something to record.
    /// let source_synth_id = 2003;
    /// server.send(server::SynthNew::new("bubbles", 1, iter::empty()).synth_id(source_synth_id))?;
    ///
    /// // Allocate a buffer for disk I/O.
    /// let buffer_number = 0;
    /// server.send_sync(server::BufferAllocate::new(buffer_number, 65536).number_of_channels(2))?;
    ///
    /// // Create an output file for this buffer and leave it open.
    /// server.send_sync(
    ///     server::BufferWrite::new(
    ///         buffer_number,
    ///         "diskout-test.wav",
    ///         server::HeaderFormat::Aiff,
    ///         server::SampleFormat::Int16,
    ///     )
    ///     .number_of_frames(0)
    ///     .leave_file_open(),
    /// )?;
    ///
    /// // Create the DiskOut node.
    /// let recording_synth_id = 2004;
    /// server.send({
    ///     let controls = vec![server::Control::new("bufnum", buffer_number)];
    ///     server::SynthNew::new("help-Diskout", source_synth_id, controls)
    ///         .synth_id(recording_synth_id)
    ///         .add_action(server::AddAction::AfterNode)
    /// })?;
    ///
    /// // Let it play for a while
    /// thread::sleep(Duration::from_secs(5));
    ///
    /// // Stop recording.
    /// server.send(server::NodeFree::new(iter::once(recording_synth_id)))?;
    /// // Stop the audio source.
    /// server.send(server::NodeFree::new(iter::once(source_synth_id)))?;
    ///
    /// // Close the file and free the buffer's memory.
    /// server.send_sync(server::BufferClose::new(buffer_number))?;
    /// server.send_sync(server::BufferFree::new(buffer_number))?;
    /// # Ok(())
    /// # }
    /// ```
    DiskOut[ar] {
        inputs: {
            bufnum: f32 = 0,
            channels: multi = 0
        },
        options: {
            num_outputs: 1
        }
    }
}

ugen! {
    /// Read signals from an audio or control bus.
    ///
    /// [`In::ar`] and [`In::kr`] read signals from audio and control buses, respectively. (See the
    /// [Buses] chapter of the [Getting Started] tutorial series for details on buses.)
    ///
    /// `In::ar` and `In::kr` behave slightly differently with respect to signals left on the bus
    /// in the previous calculation cycle.
    ///
    /// `In::ar` can access audio signals that were generated in the current calculation cycle by
    /// Synth nodes located earlier in the node tree (see [Order of execution]). It does not read
    /// signals left on an audio bus from the previous calculation cycle. If synth A reads from
    /// audio bus 0 and synth B writes to audio bus 0, and synth A is earlier than synth B, In.ar
    /// in synth A will read 0's (silence). This is to prevent accidental feedback. [`InFeedback`]
    /// supports audio signal feedback.
    ///
    /// `In::kr` is for control buses. Control signals may be generated by Synth nodes within the
    /// server, or they may be set by the client and expected to hold steady. Therefore, `In::kr`
    /// does not distinguish between "new" and "old" data: it will always read the current value on
    /// the bus, whether it was generated earlier in this calculation cycle, left over from the
    /// last one, or set by the client.
    ///
    // TODO: figure how we want to manage buses and recommend it here
    //
    // Note that using the Bus class to allocate a multichannel bus simply reserves a series of
    // adjacent bus indices with the Server object's bus allocators. abus.index simply returns the
    // first of those indices.
    //
    // When using a bus with an `In` or [`Out`] UGen there is nothing to stop you from reading to
    // or writing from a larger range, or from hardcoding to a bus that has been allocated. You are
    // responsible for making sure that the number of channels match and that there are no
    // conflicts. See the [Server Architecture] and Bus helpfiles for more information on buses and
    // how they are used.
    //
    /// The hardware input buses begin just after the hardware output buses and can be read from
    /// using `In::ar` (See [Server Architecture] for more details). The number of hardware input
    /// and output buses can vary depending on your Server's options. For a convenient wrapper
    /// class which deals with this issue see [`SoundIn`].
    ///
    /// [Buses]: https://doc.sccode.org/Tutorials/Getting-Started/11-Buses.html
    /// [Getting Started]: https://doc.sccode.org/Tutorials/Getting-Started/00-Getting-Started-With-SC.html
    /// [Order of execution]: https://doc.sccode.org/Guides/Order-of-execution.html
    /// [Server Architecture]: https://doc.sccode.org/Reference/Server-Architecture.html
    In[ar, kr] {
        inputs: {
            /// The index of the bus to read in from.
            bus: f32 = 0,
            /// The number of channels (i.e. adjacent buses) to read in. You cannot modulate this
            /// number by assigning it to an parameter in a synth definition.
            // TODO: make this take an i32 only instead of an `impl Into<Value>`
            number_of_channels: f32 = 1
        },
        options: {
            num_outputs: 1
        }
    }
}

ugen! {
    /// Number of output buses.
    NumOutputBuses[ir] {
        inputs: {},
        options: {
            num_outputs: 1
        }
    }
}

macro_rules! value_setter {
    ( $(#[$meta:meta])* $field:ident ) => {
        $(#[$meta])*
        pub fn $field(mut self, value: impl Into<Value>) -> Self {
            self.$field = value.into();
            self
        }
    };
}

macro_rules! simple_setter {
    ( $(#[$meta:meta])* $field:ident: $type:ty ) => {
        $(#[$meta])*
        pub fn $field(mut self, value: $type) -> Self {
            self.$field = value;
            self
        }
    };
}

/// An envelope generator.
///
/// Plays back break point envelopes. The envelopes are instances of the Env class. The envelope
/// and the arguments for levelScale, levelBias, and timeScale are polled when the EnvGen is
/// triggered and remain constant for the duration of the envelope.
pub struct EnvGen {
    rate: Rate,
    envelope: Env,
    gate: Value,
    level_scale: Value,
    level_bias: Value,
    time_scale: Value,
    done_action: DoneAction,
}

impl EnvGen {
    /// Create a UGen that calculates samples at audio rate.
    pub fn ar() -> EnvGen {
        EnvGen::new(Rate::Audio)
    }

    /// Create a UGen that calculates samples at control rate.
    pub fn kr() -> EnvGen {
        EnvGen::new(Rate::Control)
    }

    fn new(rate: Rate) -> EnvGen {
        EnvGen {
            rate,
            envelope: Env::default(),
            gate: 1.into(),
            level_scale: 1.into(),
            level_bias: 0.into(),
            time_scale: 1.into(),
            done_action: DoneAction::None,
        }
    }

    /// An Envelope value, or an Array of Controls.
    // (See Control and the example below for how to use this.)
    ///
    /// The envelope is polled when the `EnvGen` is triggered. The Envelope inputs can be other
    /// UGens.
    pub fn envelope(mut self, envelope: Env) -> EnvGen {
        self.envelope = envelope;
        self
    }

    /// Triggers the envelope and holds it open while > 0.
    ///
    /// If the [`Env`](envelope::Env) is fixed-length (e.g.  [`Env:linen`](envelope::Env:linen),
    /// [`Env::perc`](envelope::Env::perc)), the `gate` argument is used as a simple trigger. If it
    /// is an sustaining envelope (e.g.  [`Env::adsr`](envelope::Env::adsr),
    /// [`Env::asr`](envelope::Env::asr)), the envelope is held open until the gate becomes 0, at
    /// which point is released.
    ///
    /// If `gate` < 0, force release with time -1.0 - gate. See Forced release below.
    pub fn gate(mut self, value: impl Into<Value>) -> EnvGen {
        self.gate = value.into();
        self
    }

    /// The levels of the breakpoints are multiplied by this value
    ///
    /// This value can be modulated, but is only sampled at the start of a new envelope segment.
    pub fn level_scale(mut self, value: impl Into<Value>) -> EnvGen {
        self.level_scale = value.into();
        self
    }

    /// This value is added as an offset to the levels of the breakpoints.
    ///
    /// This value can be modulated, but is only sampled at the start of a new envelope segment.
    pub fn level_bias(mut self, value: impl Into<Value>) -> EnvGen {
        self.level_bias = value.into();
        self
    }

    /// The durations of the segments are multiplied by this value.
    ///
    /// This value can be modulated, but is only sampled at the start of a new envelope segment.
    pub fn time_scale(mut self, value: impl Into<Value>) -> EnvGen {
        self.time_scale = value.into();
        self
    }

    /// An action to be executed when the env is finished playing.
    ///
    /// This can be used to free the enclosing synth, etc. See [`DoneAction`] for more detail.
    pub fn done_action(mut self, done_action: DoneAction) -> EnvGen {
        self.done_action = done_action;
        self
    }
}

impl From<EnvGen> for Value {
    fn from(ugen: EnvGen) -> Value {
        let mut spec = UGenSpec {
            name: "EnvGen".into(),
            rate: ugen.rate,
            special_index: 0,
            inputs: vec![],
            outputs: vec![ugen.rate],
        };

        spec.inputs.push(UgenInput::Simple(ugen.gate));
        spec.inputs.push(UgenInput::Simple(ugen.level_scale));
        spec.inputs.push(UgenInput::Simple(ugen.level_bias));
        spec.inputs.push(UgenInput::Simple(ugen.time_scale));
        spec.inputs.push(UgenInput::Simple(ugen.done_action.into()));
        for value in ugen.envelope.into_values() {
            spec.inputs.push(UgenInput::Simple(value));
        }
        spec.into()
    }
}

/// Sample playback oscillator.
///
/// Plays back a sample resident in memory.
///
/// # Required Arguments
///
/// * `number_of_channels` - Number of channels that the buffer will be. This must be a fixed
///   integer. The architecture of the synth definition cannot change after it is compiled.
pub struct PlayBuf {
    ugen_rate: Rate,
    number_of_channels: usize,

    bufnum: Value,
    rate: Value,
    trigger: Value,
    start_pos: Value,
    loop_buffer: Value,
    done_action: DoneAction,
}

impl PlayBuf {
    /// Create a UGen that calculates samples at audio rate.
    pub fn ar(number_of_channels: usize) -> PlayBuf {
        PlayBuf::new(number_of_channels, Rate::Audio)
    }

    /// Create a UGen that calculates samples at control rate.
    pub fn kr(number_of_channels: usize) -> PlayBuf {
        PlayBuf::new(number_of_channels, Rate::Control)
    }

    fn new(number_of_channels: usize, rate: Rate) -> PlayBuf {
        PlayBuf {
            ugen_rate: rate,
            number_of_channels,

            bufnum: 0.0.into(),
            rate: 1.0.into(),
            trigger: 1.0.into(),
            start_pos: 0.0.into(),
            loop_buffer: 0.0.into(),
            done_action: DoneAction::None,
        }
    }

    value_setter! {
        /// The index of the buffer to use.
        ///
        /// **Note**: If you supply a buffer number of a buffer with a differing number of channels
        /// than the one specified in this [`PlayBuf`], it will post a warning and output the
        /// channels it can.
        bufnum
    }
    value_setter! {
        /// 1.0 is the server's sample rate, 2.0 is one octave up, 0.5 is one octave down -1.0 is
        /// backwards normal rate… etc. Interpolation is cubic.
        rate
    }
    value_setter! {
        /// A trigger causes a jump to the [`start_pos`](PlayBuf::start_pos). A trigger occurs when
        /// a signal changes from negative value to positive value.
        trigger
    }
    value_setter! {
        /// Sample frame to start playback.
        start_pos
    }
    value_setter! {
        /// 1 means true, 0 means false. This is modulateable.
        loop_buffer
    }
    simple_setter! {
        /// An integer representing an action to be executed when the buffer is finished playing.
        /// This can be used to free the enclosing synth, etc. See [`Done`] for more detail.
        /// `done_action` is only evaluated if loop is 0.
        done_action: DoneAction
    }
}

impl From<PlayBuf> for Value {
    fn from(ugen: PlayBuf) -> Self {
        let mut spec = UGenSpec {
            name: "PlayBuf".into(),
            rate: ugen.ugen_rate,
            special_index: 0,
            inputs: vec![],
            outputs: vec![ugen.ugen_rate; ugen.number_of_channels],
        };

        spec.inputs.push(UgenInput::Simple(ugen.bufnum));
        spec.inputs.push(UgenInput::Simple(ugen.rate));
        spec.inputs.push(UgenInput::Simple(ugen.trigger));
        spec.inputs.push(UgenInput::Simple(ugen.start_pos));
        spec.inputs.push(UgenInput::Simple(ugen.loop_buffer));
        spec.inputs.push(UgenInput::Simple(ugen.done_action.into()));
        spec.into()
    }
}

/// Read audio from a system audio device.
///
/// `SoundIn` is a convenience UGen to read audio from the input of your computer or soundcard. It
/// is a wrapper UGen based on [`In`], which offsets the index such that `0` will always correspond
/// to the first input regardless of the number of inputs present.
pub struct SoundIn {
    bus: Value,
}

impl SoundIn {
    /// Create a UGen that calculates samples at audio rate.
    pub fn ar() -> Self {
        Self { bus: 0.into() }
    }

    value_setter! {
        /// The channel (or array of channels) to read in. These start at `0`, which will
        /// correspond to the first audio input.
        bus
    }
}

impl From<SoundIn> for Value {
    fn from(ugen: SoundIn) -> Self {
        let channel_offest = NumOutputBuses::ir().into_value();
        match ugen.bus.0 {
            VecTree::Leaf(_) => In::ar()
                .bus(channel_offest + ugen.bus)
                .number_of_channels(1)
                .into_value(),
            VecTree::Branch(ref buses) => {
                let number_of_channels = buses.len();

                let is_consecutive = buses
                    .iter()
                    .map(|bus| match bus {
                        VecTree::Leaf(Scalar::Const(x)) => Some(*x),
                        _ => None,
                    })
                    .collect::<Option<Vec<f32>>>()
                    .and_then(|bus_numbers| {
                        let start = *bus_numbers.first()? as usize;
                        // replace with SuperCollider's probably more space efficient implementation
                        let is_consecutive = bus_numbers
                            == (start..(start + bus_numbers.len()))
                                .into_iter()
                                .map(|x| x as f32)
                                .collect::<Vec<_>>();
                        Some(is_consecutive)
                    })
                    .unwrap_or(false);

                let first_bus = Value(
                    buses
                        .first()
                        .expect("TODO: we check for the head twice, we could probably do better")
                        .clone(),
                );

                if is_consecutive {
                    In::ar()
                        .bus(channel_offest + first_bus)
                        .number_of_channels(number_of_channels as f32)
                        .into_value()
                } else {
                    In::ar()
                        .bus(channel_offest + ugen.bus)
                        .number_of_channels(1)
                        .into_value()
                }
            }
        }
    }
}

/// Continuously play a longer sound file from disk.
///
/// This requires a buffer to be preloaded with one buffer size of sound.
pub struct DiskIn {
    number_of_channels: usize,
    buffer_number: Value,
    loop_buffer: Value,
}

impl DiskIn {
    /// Create a UGen that calculates samples at audio rate.
    ///
    /// # Arguments
    ///
    /// * `number_of_channels` - The number of channels. This must match the number of channels in
    ///   the buffer.
    /// * `buffer_number` - The number of the buffer to use when playing the file.
    pub fn ar(number_of_channels: usize, buffer_number: impl Into<Value>) -> Self {
        Self {
            number_of_channels,
            buffer_number: buffer_number.into(),
            loop_buffer: 0.into_value(),
        }
    }

    value_setter! {
        /// Set to `1` to loop the sound file.
        loop_buffer
    }
}

impl From<DiskIn> for Value {
    fn from(ugen: DiskIn) -> Self {
        let mut spec = UGenSpec {
            name: "DiskIn".into(),
            rate: Rate::Audio,
            special_index: 0,
            inputs: Vec::with_capacity(2),
            outputs: vec![Rate::Audio; ugen.number_of_channels],
        };

        spec.inputs.push(UgenInput::Simple(ugen.buffer_number));
        spec.inputs.push(UgenInput::Simple(ugen.loop_buffer));
        spec.into()
    }
}
