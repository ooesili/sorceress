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

//! A module for defining envelopes.
//!
//! [`Env`] is the focus of this module.
use crate::{
    synthdef::{IntoValue, Scalar, Value},
    vectree::VecTree,
};

/// An specification for a segmented envelope.
// TODO: implement this
// Envs can be used both server-side, by an EnvGen or an IEnvGen within a SynthDef, and
// clientside, with methods such as -at and -asStream, below.
///
/// An `Env` can have any number of segments which can stop at a particular value or loop several
/// segments when sustaining. It can have several shapes for its segments.
///
/// The envelope is conceived as a sequence of nodes (not to be confused with a synthesis node):
/// the first node gives the initial level of the envelope, and the following have three
/// parameters: a target level, a time duration from the previous node, and a shape. The three
/// parameters for each node are kept in separate arrays as explained below.
///
///
/// ```rust
/// use sorceress::ugen::envelope::Env;
///
/// Env::default()
///     .levels(vec![0.0, 1.0, 0.9, 0.0])
///     .times(vec![0.1, 0.5, 1.0])
///     .curve(vec![-5, 0, 5]);
///
/// ```
///
/// In this envelope, there are four nodes :
///
/// * The first node is the initial level of the envelope: `0`.
/// * The second node has level `1` and is reached in `0.1` second.
/// * The third nodes has level `0.9` and is reached in `0.5` second.
/// * The fourth nodes has level `0` and is reached in `1` second.
///
/// Close attention must be paid when retriggering envelopes. Starting from their value at the
/// moment of retrigger, envelopes will cycle through all of their nodes, with the exception of the
/// first. The first node is an envelope's initial value and is only output prior to the initial
/// trigger.
pub struct Env {
    loop_node: Option<usize>,
    release_node: Option<usize>,
    levels: Vec<Value>,
    times: Vec<Value>,
    curve: CurveInput,
}

impl Env {
    pub fn adsr() -> Env {
        Env::default()
            .levels(vec![0.0, 1.0, 0.5, 0.0])
            .times(vec![0.01, 0.3, 1.0])
            .curve(-4)
            .release_node(2)
    }

    // TODO: port all of Env's constructor methods from SuperCollider

    /// Sets of levels of the envelope segments.
    ///
    /// The first value is the initial level of the envelope. When the envelope is used with an
    /// [`EnvGen`](super::EnvGen), levels can be any UGen (new level values are updated only when
    /// the envelope has reached that point). When the array of levels contains itself an array,
    /// the envelope returns a multichannel output (for a discussion, see [Multichannel
    /// Expansion]).
    ///
    /// [Multichannel Expansion]: https://doc.sccode.org/Classes/Env.html#Multichannel%20expansion
    pub fn levels<I, T>(mut self, levels: I) -> Env
    where
        I: IntoIterator<Item = T>,
        T: Into<Value>,
    {
        self.levels = levels.into_iter().map(T::into).collect();
        self
    }

    /// Sets the times, in seconds, of the envelope segments.
    ///
    /// There should be one fewer duration than there are [`levels`](Env::levels), but if shorter,
    /// the list is extended by wrapping around the given values. If there are extra durations,
    /// they will be ignored.
    pub fn times<I, T>(mut self, times: I) -> Env
    where
        I: IntoIterator<Item = T>,
        T: Into<Value>,
    {
        self.times = times.into_iter().map(T::into).collect();
        self
    }

    /// Sets the curves of the envelope segments.
    ///
    /// Determines the shapes of the envelope segments. A value of any of the following types may
    /// be given:
    ///
    /// * [`i32`] or [`f32`] - a curvature value for all segments. `0` means linear, positive and
    ///   negative numbers curve the segment up and down.
    /// * [`Curve`] - The specification of a curve. See [`Curve`] for more details.
    /// * [`Value`] - Any value usable in a synth definition, including a UGen.
    /// * A [`Vec`] of any of the above - The curvature values for each segment.
    pub fn curve(mut self, curve: impl Into<CurveInput>) -> Env {
        self.curve = curve.into();
        self
    }

    /// Creates a segment of looping nodes.
    ///
    /// You must specify a [`release_node`](Env::release_node) in order for `loop_node` to have any
    /// effect. The loop node is the initial node of the loop and is never repeated. Upon
    /// reaching the release node, the envelope will move back to the node that immediately
    /// follows loop node. The envelope will loop until its gate is closed. When released, a
    /// looping envelope will move from its current position to the node that immediately follows
    /// release node and continue until the end.
    ///
    /// # Examples
    ///
    /// ```
    /// use sorceress::ugen::{self, envelope::Env};
    ///
    /// ugen::EnvGen::kr()
    ///     .envelope(
    ///         Env::default()
    ///             .levels(vec![0.0, 1.0, 0.0, 0.2, 0.0, 0.5, 0.8, 0.0])
    ///             .times(vec![0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01])
    ///             .release_node(5)
    ///             .loop_node(1)
    ///     )
    ///     .gate(ugen::Trig::kr().input(1).duration(0.1));
    /// ```
    pub fn loop_node(mut self, index: usize) -> Env {
        self.loop_node.replace(index);
        self
    }

    /// Sustains the envelope until released.
    ///
    /// If specified, the envelope will hold at the given segment `index` until it is released. The
    /// envelope is released when the gate of the [`EnvGen`](super::EnvGen) UGen using this
    /// envelope becomes zero.
    ///
    /// # Examples
    ///
    /// ```
    /// use sorceress::ugen::{self, envelope::Env};
    ///
    /// ugen::EnvGen::kr()
    ///     .envelope(
    ///         Env::default()
    ///             .levels(vec![0.0, 1.0, 0.5, 0.0])
    ///             .times(vec![0.01, 0.01, 0.01])
    ///             // sustains at level 0.5 until gate is closed
    ///             .release_node(2),
    ///     )
    ///     .gate(
    ///         ugen::Trig::kr()
    ///             .input(ugen::Impulse::kr().freq(3))
    ///             .duration(0.3),
    ///     );
    /// ```
    pub fn release_node(mut self, index: usize) -> Env {
        self.release_node.replace(index);
        self
    }

    pub(crate) fn into_values(self) -> Vec<Value> {
        if self.levels.is_empty() {
            panic!("no levels set on envelope")
        }
        if self.levels.len() > 1 && self.times.is_empty() {
            panic!("multiple levels set on envelope, but no times were set")
        }

        let size = self.times.len();
        let mut values = Vec::with_capacity((size + 1) * 4);
        let mut levels = self.levels.into_iter();
        values.push(levels.next().unwrap());
        values.push((size as i32).into());
        values.push(node_index_to_value(self.release_node));
        values.push(node_index_to_value(self.loop_node));

        let curves = match self.curve.0 {
            VecTree::Leaf(curve) => vec![VecTree::Leaf(curve)],
            VecTree::Branch(xs) => xs,
        };

        let curve_shapes = curves
            .clone()
            .into_iter()
            .map(|tree| Value(tree.map(ExpandedCurve::shape)));

        let curve_values = curves
            .into_iter()
            .map(|tree| Value(tree.map(|curve| curve.value())));

        for (((level, time), curve_shape), curve_value) in levels
            .into_iter()
            .zip(self.times.into_iter().cycle())
            .zip(curve_shapes.cycle())
            .zip(curve_values.cycle())
        {
            values.push(level);
            values.push(time);
            values.push(curve_shape);
            values.push(curve_value);
        }

        values
    }
}

impl Default for Env {
    /// Returns a basic linear envelope with 3 nodes.
    ///
    /// Returns an envelope with a linear curve that goes from `0` to `1` in one second the back
    /// down to `0` in another second.
    fn default() -> Env {
        Env {
            loop_node: None,
            release_node: None,
            levels: vec![0.into_value(), 1.into_value(), 0.into_value()],
            times: vec![1.into_value(), 1.into_value()],
            curve: CurveInput::from(Curve::Linear),
        }
    }
}

fn node_index_to_value(index: Option<usize>) -> Value {
    index.map(|index| index as f32).unwrap_or(-99.0).into()
}

/// The curve of an envelope segment.
///
/// See [`Env::curve`] for more details.
#[derive(Debug, Clone)]
pub enum Curve {
    /// Flat segments (immediately jumps to final value).
    Step,
    /// Flat segments (holds initial value, jump to final value at the end of the segment).
    Hold,
    /// Linear segments, the default for new envelopes.
    Linear,
    /// Natural exponential growth and decay. In this case, the levels must all be nonzero and have
    /// the same sign.
    Exponential,
    /// Sinusoidal S shaped segments.
    Sine,
    /// Sinusoidal segments shaped like the sides of a Welch window.
    Welch,
    /// Squared segments.
    Squared,
    /// Cubed segments.
    Cubed,
    /// A curvature value. 0 means linear, positive and negative numbers curve the segment up and
    /// down.
    Curve(Value),
}

/// An input to [`Env::curve`].
///
/// See [`Env::curve`] for more details.
pub struct CurveInput(VecTree<ExpandedCurve>);

impl<T> From<Vec<T>> for CurveInput
where
    T: Into<CurveInput>,
{
    fn from(inputs: Vec<T>) -> CurveInput {
        CurveInput(VecTree::Branch(
            inputs.into_iter().map(|input| input.into().0).collect(),
        ))
    }
}

impl From<Curve> for CurveInput {
    fn from(curve: Curve) -> CurveInput {
        CurveInput(match curve {
            Curve::Step => VecTree::Leaf(ExpandedCurve::Step),
            Curve::Linear => VecTree::Leaf(ExpandedCurve::Linear),
            Curve::Exponential => VecTree::Leaf(ExpandedCurve::Exponential),
            Curve::Sine => VecTree::Leaf(ExpandedCurve::Sine),
            Curve::Welch => VecTree::Leaf(ExpandedCurve::Welch),
            Curve::Curve(value) => value.0.map(ExpandedCurve::Curve),
            Curve::Squared => VecTree::Leaf(ExpandedCurve::Squared),
            Curve::Cubed => VecTree::Leaf(ExpandedCurve::Cubed),
            Curve::Hold => VecTree::Leaf(ExpandedCurve::Hold),
        })
    }
}

impl From<f32> for CurveInput {
    fn from(curve: f32) -> CurveInput {
        Curve::Curve(curve.into()).into()
    }
}

impl From<i32> for CurveInput {
    fn from(curve: i32) -> CurveInput {
        Curve::Curve(curve.into()).into()
    }
}

#[derive(Debug, Clone)]
enum ExpandedCurve {
    Step,
    Linear,
    Exponential,
    Sine,
    Welch,
    Curve(Scalar),
    Squared,
    Cubed,
    Hold,
}

impl ExpandedCurve {
    fn shape(self) -> Scalar {
        Scalar::Const(match self {
            ExpandedCurve::Step => 0.0,
            ExpandedCurve::Linear => 1.0,
            ExpandedCurve::Exponential => 2.0,
            ExpandedCurve::Sine => 3.0,
            ExpandedCurve::Welch => 4.0,
            ExpandedCurve::Curve(_) => 5.0,
            ExpandedCurve::Squared => 6.0,
            ExpandedCurve::Cubed => 7.0,
            ExpandedCurve::Hold => 8.0,
        })
    }

    fn value(self) -> Scalar {
        match self {
            ExpandedCurve::Step => Scalar::Const(0.0),
            ExpandedCurve::Linear => Scalar::Const(0.0),
            ExpandedCurve::Exponential => Scalar::Const(0.0),
            ExpandedCurve::Sine => Scalar::Const(0.0),
            ExpandedCurve::Welch => Scalar::Const(0.0),
            ExpandedCurve::Curve(scalar) => scalar,
            ExpandedCurve::Squared => Scalar::Const(0.0),
            ExpandedCurve::Cubed => Scalar::Const(0.0),
            ExpandedCurve::Hold => Scalar::Const(0.0),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_value_should_match_sclang() {
        let expected = vec![0, 2, -99, -99, 1, 1, 1, 0, 0, 1, 1, 0]
            .into_iter()
            .map(|x| Value::from(x))
            .collect::<Vec<_>>();
        assert_eq!(expected, Env::default().into_values());
    }

    #[test]
    fn adsr_default_should_match_sclang() {
        let expected = vec![
            0.0, 3.0, 2.0, -99.0, 1.0, 0.01, 5.0, -4.0, 0.5, 0.3, 5.0, -4.0, 0.0, 1.0, 5.0, -4.0,
        ]
        .into_iter()
        .map(|x| Value::from(x))
        .collect::<Vec<_>>();
        assert_eq!(expected, Env::adsr().into_values());
    }
}
