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

//! Core types for creating synth definitions.
//!
//! Synth definitions are used by SuperCollider to create new synths. Synth definitions are formed
//! from directed acyclic graphs (DAGs) of UGens. UGens are primitives offered by SuperCollider
//! that generate and process sound.
//!
//! # Examples
//!
//! ```
//! use sorceress::{
//!     synthdef::SynthDef,
//!     ugen::{Out, Pan2, SinOsc},
//! };
//!
//! let synthdef = SynthDef::new("example", |params| {
//!     let freq = params.named("freq", 440.0);
//!     let pan = params.named("pan", 0.0);
//!     Out::ar().channels(Pan2::ar().input(SinOsc::ar().freq(freq)).pos(pan))
//! });
//! ```
use crate::vectree::VecTree;
use std::sync::Arc;

pub mod decoder;
pub mod encoder;

// IDEA: parameter to control ugen as a discrete phase

/// A named synth definition.
///
/// Synth definitions are used by SuperCollider to create new synths.
#[derive(Debug, Clone)]
pub struct SynthDef {
    name: String,
    graph: VecTree<Scalar>,
    params: Parameters,
}

impl SynthDef {
    /// Creates a new synth definition.
    ///
    /// The name given here is used when creating new synths after this synthdef definition has be
    /// registered with the server. This method does not register the synth definition with the
    /// SuperCollider server which must happen before synths can be created from it.
    ///
    /// You can refer to parameters by index or name in many server commands, The `Parameters`
    /// passed to the `ugen_fn` allow you to control the order of parameters in the synth
    /// definition. The order of calls to methods on passed [`Parameters`] determines the index of
    /// each parameter. For that reason it's recommend to declare all parameters up front at the
    /// top of the `ugen_fn`.
    ///
    /// # Examples
    ///
    /// ```
    /// use sorceress::{
    ///     synthdef::{Input, SynthDef},
    ///     ugen,
    /// };
    ///
    /// let synthdef = SynthDef::new("example", |params| {
    ///     let freq = params.named("name", 440.0); // index 0
    ///     let vol = params.named("vol", 1.0); // index 1
    ///     ugen::Out::ar().channels(ugen::SinOsc::ar().freq(freq).mul(vol))
    /// });
    /// ```
    pub fn new<F, T>(name: impl Into<String>, ugen_fn: F) -> SynthDef
    where
        F: FnOnce(&mut Parameters) -> T,
        T: Input,
    {
        let mut params = Parameters::empty();
        let graph = ugen_fn(&mut params).into_value().0;
        SynthDef {
            name: name.into(),
            graph,
            params,
        }
    }

    /// Returns the name of the synth definition.
    pub fn name(&self) -> &str {
        &self.name
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct UGenSpec<Input> {
    pub name: String,
    pub rate: Rate,
    pub special_index: i16,
    pub inputs: Vec<Input>,
    pub outputs: Vec<Rate>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, PartialOrd, Ord)]
pub(crate) enum Rate {
    Scalar = 0,
    Control = 1,
    Audio = 2,
}

impl From<Rate> for i8 {
    fn from(rate: Rate) -> Self {
        rate as i8
    }
}

/// An action to invoke when a UGen is finished playing.
///
/// A number of UGens implement "done actions". These allow one to optionally free or pause the
/// enclosing synth and other related nodes when the UGen is finished.
pub enum DoneAction {
    /// Do nothing when the UGen is finished.
    None = 0,
    /// Pause the enclosing synth, but do not free it.
    PauseSelf = 1,
    /// Free the enclosing synth.
    FreeSelf = 2,
    /// Free both this synth and the preceding node.
    FreeSelfAndPrev = 3,
    /// Free both this synth and the following node.
    FreeSelfAndNext = 4,
    /// Free this synth; if the preceding node is a group then do g_freeAll on it, else free it.
    FreeSelfAndFreeAllInPrev = 5,
    /// Free this synth; if the following node is a group then do g_freeAll on it, else free it.
    FreeSelfAndFreeAllInNext = 6,
    /// Free this synth and all preceding nodes in this group.
    FreeSelfToHead = 7,
    /// Free this synth and all following nodes in this group.
    FreeSelfToTail = 8,
    /// Free this synth and pause the preceding node.
    FreeSelfPausePrev = 9,
    /// Free this synth and pause the following node.
    FreeSelfPauseNext = 10,
    /// Free this synth and if the preceding node is a group then deep free it, else free it.
    FreeSelfAndDeepFreePrev = 11,
    /// Free this synth and if the following node is a group then deep free it, else free it.
    FreeSelfAndDeepFreeNext = 12,
    /// Free this synth and all other nodes in this group (before and after).
    FreeAllInGroup = 13,
    /// Free the enclosing group and all nodes within it (including this synth).
    FreeGroup = 14,
    /// Free this synth and resume the following node.
    FreeSelfResumeNext = 15,
}

impl Input for DoneAction {
    fn into_value(self) -> Value {
        (self as i32).into_value()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Scalar {
    Const(f32),
    Parameter(Parameter),
    Ugen {
        output_index: i32,
        ugen_spec: Arc<UGenSpec<Scalar>>,
    },
}

impl Scalar {
    fn rate(&self) -> Rate {
        match self {
            Self::Const(_) => Rate::Scalar,
            Self::Parameter(_) => Rate::Control,
            Self::Ugen {
                ugen_spec,
                output_index,
            } => ugen_spec.outputs[*output_index as usize],
        }
    }
}

/// A factory for creating parameters in a synth definition.
///
/// A value of this type is passed to the `ugen_fn` closure given to [`SynthDef::new`]. See
/// [`SynthDef::new`] for more details.
#[derive(Debug, Clone)]
pub struct Parameters {
    initial_values: Vec<f32>,
    names: Vec<(String, usize)>,
}

impl Parameters {
    fn empty() -> Parameters {
        Parameters {
            initial_values: Vec::new(),
            names: Vec::new(),
        }
    }

    /// Defines a parameter within a synth definition.
    ///
    /// Creates a new named parameter with an initial value. If a value is not specified for this
    /// parameter when creating a new synth, the initial value will be used.
    pub fn named(&mut self, name: impl Into<String>, initial_value: f32) -> Parameter {
        let index = self.initial_values.len();
        self.initial_values.push(initial_value);
        self.names.push((name.into(), index));
        Parameter { index }
    }
}

/// A synth definition parameter.
///
/// Parameters allow synths to be controlled externally.
///
/// # Examples
/// ```
/// use sorceress::{
///     synthdef::SynthDef,
///     ugen::{Out, Pan2, SinOsc},
/// };
///
/// fn example_synth_def() -> SynthDef {
///     SynthDef::new("example", |params| {
///         let freq = params.named("freq", 440.0);
///         let pan = params.named("pan", 0.0);
///         Out::ar().channels(Pan2::ar().input(SinOsc::ar().freq(freq)).pos(pan))
///     })
/// }
/// ```
#[derive(Debug, PartialEq, Clone)]
pub struct Parameter {
    index: usize,
}

impl Input for Parameter {
    fn into_value(self) -> Value {
        Value(VecTree::Leaf(Scalar::Parameter(self)))
    }
}

/// A value in a UGen graph.
///
/// Many different types can be converted into a `Value` using the [`Input`] trait, but there are
/// only 3 general kinds of values:
///
/// * Constants
/// * Parameters
/// * UGens
///
/// *Constants* are numeric values, either an [`i32`] or [`f32`], that are hardcoded into a synth
/// definition. Constant values cannot be changed later when creating a synth, for that you must
/// use a parameter.
///
/// *Parameters* are numeric values that can be controlled externally using server commands.
/// Parameters can be used to invoke a single synth definition in different ways, such as by
/// controlling the pitch or volume of the synth.
///
/// *UGens* are the building blocks of synth definitions. UGens are primitives that generate and
/// process audio and control signals. SuperCollider provides hundreds of UGens, all of which can
/// be found in the [`ugen`](super::ugen) module.
#[derive(Debug, PartialEq, Clone)]
pub struct Value(pub(crate) VecTree<Scalar>);

impl Value {
    pub fn unwrap_stereo(self) -> (Value, Value) {
        match self.0 {
            VecTree::Leaf(_) => panic!("called `VecTree::unwrap_stereo` on a `Scalar` value"),
            VecTree::Branch(mut branch) => {
                if branch.len() != 2 {
                    panic!(
                        "called `VecTree::unwrap_stereo` on a signal with {} channels",
                        branch.len()
                    );
                }
                let b = branch.pop().unwrap();
                let a = branch.pop().unwrap();
                (Value(a), Value(b))
            }
        }
    }
}

impl Input for Value {
    fn into_value(self) -> Value {
        self
    }
}

impl Input for f32 {
    fn into_value(self) -> Value {
        Value(VecTree::Leaf(Scalar::Const(self)))
    }
}

impl Input for i32 {
    fn into_value(self) -> Value {
        Value(VecTree::Leaf(Scalar::Const(self as f32)))
    }
}

impl Input for usize {
    fn into_value(self) -> Value {
        Value(VecTree::Leaf(Scalar::Const(self as f32)))
    }
}

fn bin_op_ugen(special_index: i16, lhs: Value, rhs: Value) -> Value {
    let inputs = vec![UGenInput::Simple(lhs), UGenInput::Simple(rhs)];
    expand_inputs_with(inputs, &mut |inputs: Vec<Scalar>| {
        let rate = input_rate(&inputs);
        VecTree::Leaf(Scalar::Ugen {
            output_index: 0,
            ugen_spec: Arc::new(UGenSpec {
                name: "BinaryOpUGen".into(),
                rate,
                special_index,
                inputs,
                outputs: vec![rate],
            }),
        })
    })
}

fn mul_add(value: impl Input, mul: impl Input, add: impl Input) -> Value {
    let inputs = vec![
        UGenInput::Simple(value.into_value()),
        UGenInput::Simple(mul.into_value()),
        UGenInput::Simple(add.into_value()),
    ];
    expand_inputs_with(inputs, &mut |inputs: Vec<Scalar>| {
        let rate = input_rate(&inputs);
        VecTree::Leaf(Scalar::Ugen {
            output_index: 0,
            ugen_spec: Arc::new(UGenSpec {
                name: "MulAdd".to_owned(),
                rate,
                special_index: 0,
                inputs,
                outputs: vec![rate],
            }),
        })
    })
}

fn unary_op_ugen(special_index: i16, input: impl Input) -> Value {
    let inputs = vec![UGenInput::Simple(input.into_value())];
    expand_inputs_with(inputs, &mut |inputs: Vec<Scalar>| {
        let rate = input_rate(&inputs);
        VecTree::Leaf(Scalar::Ugen {
            output_index: 0,
            ugen_spec: Arc::new(UGenSpec {
                name: "UnaryOpUGen".to_owned(),
                rate,
                special_index,
                inputs,
                outputs: vec![rate],
            }),
        })
    })
}

fn input_rate(inputs: &[Scalar]) -> Rate {
    inputs
        .iter()
        .map(|input| input.rate())
        .max()
        .unwrap_or(Rate::Scalar)
}

/// A trait for values that can be used in UGen graphs.
///
/// This trait is primarily used as a bound on the arguments to UGen structs. This allows for a
/// wide range of types to be given as parameters. `Input` is implemented by:
///
/// * Numeric types - [`i32`], [`f32`], and [`usize`]
/// * Synth definition parameters - [`Parameter`]
/// * All UGen structs
/// * [`Value`]
/// * A [`Vec`] of other inputs
///
/// If a vector of `Input`s is passed to a unit generator, multichannel expansion will be applied.
///
/// # Examples
///
/// Multichannel expansion:
/// ```
/// use sorceress::{synthdef::SynthDef, ugen};
///
/// // The following two synth definitions are equivalent.
///
/// let synthdef1 = SynthDef::new("multichannel", |_| {
///     ugen::Out::ar().channels(ugen::SinOsc::ar().freq(vec![440, 220]))
/// });
///
/// let synthdef2 = SynthDef::new("multichannel", |_| {
///     ugen::Out::ar().channels(vec![
///         ugen::SinOsc::ar().freq(440),
///         ugen::SinOsc::ar().freq(220),
///     ])
/// });
/// ```
pub trait Input: Sized {
    /// Converts the input into a `Value`.
    fn into_value(self) -> Value;

    /// Adds an `Input` to another.
    fn add(self, rhs: impl Input) -> Value {
        bin_op_ugen(0, self.into_value(), rhs.into_value())
    }

    /// Subtracts an `Input` from another.
    fn sub(self, rhs: impl Input) -> Value {
        bin_op_ugen(1, self.into_value(), rhs.into_value())
    }

    /// Multiplies an `Input` by another.
    fn mul(self, rhs: impl Input) -> Value {
        bin_op_ugen(2, self.into_value(), rhs.into_value())
    }

    /// Divides an `Input` by another.
    fn div(self, rhs: impl Input) -> Value {
        bin_op_ugen(3, self.into_value(), rhs.into_value())
    }

    /// Efficiently multiplies the signal by `mul` and adds `add`.
    ///
    /// Uses the `MulAdd` UGen under the hood.
    fn madd(self, mul: impl Input, add: impl Input) -> Value {
        mul_add(self, mul, add)
    }

    /// Converts midi note numbers into cycles per seconds (Hz).
    fn midicps(self) -> Value {
        unary_op_ugen(17, self)
    }

    /// Converts cycles per seconds (Hz) into midi note numbers.
    fn cpsmidi(self) -> Value {
        unary_op_ugen(18, self)
    }

    // TODO: add all unary operators
}

impl<T> Input for Vec<T>
where
    T: Input,
{
    fn into_value(self) -> Value {
        Value(VecTree::Branch(
            self.into_iter().map(|value| value.into_value().0).collect(),
        ))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum UGenInput {
    Simple(Value),
    Multi(Value),
}

impl UGenInput {
    fn expand(self) -> Vec<VecTree<Scalar>> {
        match self {
            UGenInput::Simple(Value(value)) => vec![value],
            UGenInput::Multi(Value(value)) => match value {
                VecTree::Leaf(expanded_value) => vec![VecTree::Leaf(expanded_value)],
                VecTree::Branch(xs) => xs,
            },
        }
    }
}

fn mutlichannel_expand<I, F, U, A, B>(inputs: I, expand_one: F) -> VecTree<Vec<B>>
where
    I: IntoIterator<Item = A>,
    F: Fn(A) -> U,
    U: IntoIterator<Item = VecTree<B>>,
    B: Clone,
{
    let expanded_inputs = inputs.into_iter().flat_map(expand_one).collect::<Vec<_>>();
    let dimensions = VecTree::space(&expanded_inputs);
    transmute_trees(&expanded_inputs, &dimensions, &mut vec![])
}

fn transmute_trees<T>(
    input_trees: &[VecTree<T>],
    dimensions: &[usize],
    path: &mut Vec<usize>,
) -> VecTree<Vec<T>>
where
    T: Clone,
{
    match dimensions {
        [] => {
            let xs = input_trees
                .iter()
                .map(|tree| tree.get_path(path).unwrap().clone())
                .collect();
            VecTree::Leaf(xs)
        }
        [size, dimensions @ ..] => {
            let mut trees = vec![];
            for i in 0..*size {
                path.push(i);
                trees.push(transmute_trees(input_trees, dimensions, path));
                path.pop();
            }
            VecTree::Branch(trees)
        }
    }
}

fn expand_inputs_with<F>(inputs: Vec<UGenInput>, f: &mut F) -> Value
where
    F: FnMut(Vec<Scalar>) -> VecTree<Scalar>,
{
    Value(mutlichannel_expand(inputs, UGenInput::expand).flat_map(f))
}

impl Input for UGenSpec<UGenInput> {
    fn into_value(self) -> Value {
        let name = self.name;
        let rate = self.rate;
        let special_index = self.special_index;
        let outputs = self.outputs;

        expand_inputs_with(self.inputs, &mut |inputs| {
            let ugen_spec = Arc::new(UGenSpec {
                name: name.clone(),
                rate,
                special_index,
                inputs,
                outputs: outputs.clone(),
            });
            if outputs.len() <= 1 {
                VecTree::Leaf(Scalar::Ugen {
                    output_index: 0,
                    ugen_spec,
                })
            } else {
                VecTree::Branch(
                    (0..outputs.len())
                        .into_iter()
                        .map(|output_index| {
                            VecTree::Leaf(Scalar::Ugen {
                                output_index: output_index as i32,
                                ugen_spec: ugen_spec.clone(),
                            })
                        })
                        .collect::<Vec<_>>(),
                )
            }
        })
    }
}
