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
//!     synthdef::{IntoValue, Parameter, SynthDef, Value},
//!     ugen::{Out, SinOsc},
//! };
//!
//! fn example_synth_def() -> SynthDef {
//!     let freq = Parameter::new("freq", 440.0).into_value();
//!     SynthDef::new(
//!         "example",
//!         Out::ar().channels(Value::from(
//!             SinOsc::ar().freq(vec![freq.clone(), 2.0.into_value() * freq]),
//!         )),
//!     )
//! }
//! ```
use crate::vectree::VecTree;
use std::{
    ops::{Add, Div, Mul, Sub},
    sync::Arc,
};

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
}

impl SynthDef {
    /// Creates a new synth definition.
    ///
    /// The name given here is used when creating new synths after this synthdef definition has be
    /// registered with the server. This method does not register the synth definition with the
    /// SuperCollider server which must happen before synths can be created from it.
    pub fn new(name: impl Into<String>, graph: impl Into<Value>) -> Self {
        Self {
            name: name.into(),
            graph: graph.into().0,
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

impl From<DoneAction> for Value {
    fn from(done_action: DoneAction) -> Self {
        (done_action as i32).into()
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

/// A synth definition parameter.
///
/// Parameters allow synths to be controlled externally.
///
/// # Examples
/// ```
/// use sorceress::{
///     synthdef::{IntoValue, Parameter, SynthDef},
///     ugen::{Out, Pan2, SinOsc},
/// };
///
/// fn example_synth_def() -> SynthDef {
///     let freq = Parameter::new("freq", 440.0);
///     let pan = Parameter::new("pan", 0.0);
///     SynthDef::new(
///         "example",
///         Out::ar().channels(Pan2::ar().input(SinOsc::ar().freq(freq)).pos(pan)),
///     )
/// }
/// ```
#[derive(Debug, PartialEq, Clone)]
pub struct Parameter {
    name: String,
    initial_value: f32,
}

impl Parameter {
    /// Creates a new parameter with an initial value. If a value is not specified for this
    /// parameter when creating a new synth, the initial value will be used.
    pub fn new(name: impl AsRef<str>, initial_value: f32) -> Self {
        Self {
            name: name.as_ref().to_owned(),
            initial_value,
        }
    }

    /// Creates a new parameter with an initial value of zero.
    ///
    /// This is simply a convenience function around [`Parameter::new`].
    pub fn zero(name: impl AsRef<str>) -> Self {
        Self::new(name, 0.0)
    }
}

impl From<Parameter> for Value {
    fn from(parameter: Parameter) -> Self {
        Value(VecTree::Leaf(Scalar::Parameter(parameter)))
    }
}

/// A value in a UGen graph.
///
/// Many different types can be converted into a `Value`, but there are only 3 general kinds of
/// values:
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

impl From<f32> for Value {
    fn from(n: f32) -> Self {
        Value(VecTree::Leaf(Scalar::Const(n)))
    }
}

impl From<i32> for Value {
    fn from(n: i32) -> Self {
        Value(VecTree::Leaf(Scalar::Const(n as f32)))
    }
}

impl From<usize> for Value {
    fn from(n: usize) -> Self {
        Value(VecTree::Leaf(Scalar::Const(n as f32)))
    }
}

fn bin_op_ugen(special_index: i16, lhs: Value, rhs: Value) -> Value {
    let inputs = vec![UGenInput::Simple(lhs), UGenInput::Simple(rhs)];
    expand_inputs_with(inputs, &mut |inputs: Vec<Scalar>| {
        let rate = inputs
            .iter()
            .map(|input| input.rate())
            .max()
            .unwrap_or(Rate::Scalar);
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

/// Adds a Value to another.
impl Add<Value> for Value {
    type Output = Value;

    fn add(self, rhs: Value) -> Self::Output {
        bin_op_ugen(0, self, rhs)
    }
}

/// Subtracts a value from another.
impl Sub<Value> for Value {
    type Output = Value;

    fn sub(self, rhs: Value) -> Self::Output {
        bin_op_ugen(1, self, rhs)
    }
}

/// Multiplies a Value by another.
impl Mul<Value> for Value {
    type Output = Value;

    fn mul(self, rhs: Value) -> Self::Output {
        bin_op_ugen(2, self, rhs)
    }
}

/// Divides a Value by another.
impl Div<Value> for Value {
    type Output = Value;

    fn div(self, rhs: Value) -> Self::Output {
        bin_op_ugen(4, self, rhs)
    }
}

impl<T> From<Vec<T>> for Value
where
    T: Into<Value>,
{
    fn from(channels: Vec<T>) -> Self {
        Value(VecTree::Branch(
            channels.into_iter().map(|value| value.into().0).collect(),
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

impl From<UGenSpec<UGenInput>> for Value {
    fn from(ugen_spec: UGenSpec<UGenInput>) -> Value {
        let name = ugen_spec.name;
        let rate = ugen_spec.rate;
        let special_index = ugen_spec.special_index;
        let outputs = ugen_spec.outputs;

        expand_inputs_with(ugen_spec.inputs, &mut |inputs| {
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

/// A conversion into a Value that consumes the input.
///
/// This can help convert things into Values in contexts where Into::into is amibguous, such as
/// when calling a method on Value immediately after the conversion.
pub trait IntoValue {
    /// Performs the conversion.
    fn into_value(self) -> Value;
}

impl<T> IntoValue for T
where
    T: Into<Value>,
{
    fn into_value(self) -> Value {
        self.into()
    }
}
