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

//! Serializes synth definitions into the format accepted by SuperCollider.

use super::{Parameters, Rate, Scalar, SignalRange, SynthDef, UGenSpec};
use std::io::Write;
use std::sync::Arc;

/// Encodes a set of synth definitions into the [SynthDef2 file format].
///
/// Synth definition files are read by the SuperCollider server and define collections of unit
/// generators and their connections.
///
/// [SynthDef2 file format]: https://doc.sccode.org/Reference/Synth-Definition-File-Format.html
///
/// # Examples
///
/// ```
/// use sorceress::{
///     synthdef::{encoder::encode_synth_defs, SynthDef, Value},
///     ugen::{Out, SinOsc},
/// };
///
/// let synth_def = SynthDef::new("example", |_| Out::ar().channels(SinOsc::ar().freq(440)));
/// let encoded_file = encode_synth_defs(vec![synth_def]);
/// ```
pub fn encode_synth_defs(synth_defs: impl IntoIterator<Item = SynthDef>) -> Vec<u8> {
    let expanded_synths = synth_defs
        .into_iter()
        .map(|synthdef| {
            (
                synthdef.name,
                synthdef.graph.into_iter().collect::<Vec<_>>(),
                synthdef.params,
            )
        })
        .collect::<Vec<_>>();
    let graphs = expanded_synths
        .iter()
        .map(|(name, expanded_inputs, params)| SynthDefGraph::new(name, &expanded_inputs, params))
        .collect();

    let mut buffer = Vec::new();
    encode_graphs(graphs, &mut buffer);
    buffer
}

struct SynthDefGraph<'a> {
    name: String,
    constants: Vec<Const>,
    initial_parameter_values: &'a [f32],
    parameter_names: &'a [(String, usize)],
    ugens: Vec<SynthDefUgen<'a>>,
    variant_specs: Vec<VariantSpec<'a>>,
}

impl<'a> SynthDefGraph<'a> {
    fn new(name: impl Into<String>, inputs: &'a [Scalar], params: &'a Parameters) -> Self {
        let mut file = SynthDefGraph {
            name: name.into(),
            constants: Vec::new(),
            initial_parameter_values: &params.initial_values,
            parameter_names: &params.names,
            ugens: Vec::new(),
            variant_specs: Vec::new(),
        };
        if !params.initial_values.is_empty() {
            file.ugens.push(SynthDefUgen {
                source: Arc::new(UGenSpec {
                    name: "Control".to_owned(),
                    rate: Rate::Audio,
                    signal_range: SignalRange::Bipolar,
                    special_index: 0,
                    inputs: vec![],
                    outputs: vec![Rate::Control; params.initial_values.len()],
                }),
                spec: UgenSpecRef {
                    name: "Control",
                    rate: Rate::Control.into(),
                    special_index: 0,
                    input_specs: vec![],
                    output_specs: vec![Rate::Control.into(); params.initial_values.len()],
                },
            })
        }
        for value in inputs.iter() {
            file.encode(value);
        }
        file
    }

    fn encode(&mut self, input: &'a Scalar) -> (i32, i32) {
        match input {
            Scalar::Const(n) => {
                let constant = Const(*n);
                let index = self
                    .constants
                    .iter()
                    .position(|x| x == &constant)
                    .unwrap_or_else(|| {
                        self.constants.push(constant);
                        self.constants.len() - 1
                    });
                (-1, index as i32)
            }
            Scalar::Parameter(ref parameter) => {
                // The control UGen is always first, and it only exists if there are parameters.
                (0, parameter.index as i32)
            }
            Scalar::Ugen {
                output_index,
                ugen_spec,
            } => {
                let index = self
                    .ugens
                    .iter()
                    .position(|u| Arc::ptr_eq(&u.source, ugen_spec))
                    .unwrap_or_else(|| {
                        let input_specs = ugen_spec
                            .inputs
                            .iter()
                            .map(|input| self.encode(input))
                            .collect();
                        self.ugens.push(SynthDefUgen {
                            source: ugen_spec.clone(),
                            spec: UgenSpecRef {
                                name: &ugen_spec.name,
                                rate: ugen_spec.rate.into(),
                                special_index: ugen_spec.special_index,
                                input_specs,
                                output_specs: ugen_spec
                                    .outputs
                                    .iter()
                                    .map(|&rate| rate.into())
                                    .collect(),
                            },
                        });
                        self.ugens.len() - 1
                    });
                (index as i32, *output_index)
            }
        }
    }
}

#[derive(PartialEq)]
struct Const(f32);

#[derive(Debug)]
struct SynthDefUgen<'a> {
    source: Arc<UGenSpec<Scalar>>,
    spec: UgenSpecRef<'a>,
}

#[derive(Debug, PartialEq)]
struct UgenSpecRef<'a> {
    name: &'a str,
    rate: i8,
    special_index: i16,
    input_specs: Vec<(i32, i32)>,
    output_specs: Vec<i8>,
}

#[derive(Debug)]
struct VariantSpec<'a> {
    name: &'a str,
    parameter_values: Vec<f32>,
}

fn encode_graphs(graphs: Vec<SynthDefGraph>, out: &mut Vec<u8>) {
    let header: i32 = 0x53436766; // hex("SCgf")
    write_i32(out, header);

    let file_version: i32 = 2;
    write_i32(out, file_version);

    write_i16(out, graphs.len() as i16);
    for graph in graphs {
        write_pstring(out, &graph.name);
        write_i32(out, graph.constants.len() as i32);
        for constant in graph.constants {
            write_f32(out, constant.0);
        }

        write_i32(out, graph.initial_parameter_values.len() as i32);
        for initial_value in graph.initial_parameter_values.iter() {
            write_f32(out, *initial_value);
        }

        write_i32(out, graph.parameter_names.len() as i32);
        for (parameter_name, parameter_index) in graph.parameter_names.iter() {
            write_pstring(out, &parameter_name);
            write_i32(out, *parameter_index as i32);
        }

        write_i32(out, graph.ugens.len() as i32);
        for ugen_spec in graph.ugens.into_iter().map(|u| u.spec) {
            write_pstring(out, ugen_spec.name);
            write_i8(out, ugen_spec.rate);
            write_i32(out, ugen_spec.input_specs.len() as i32);
            write_i32(out, ugen_spec.output_specs.len() as i32);
            write_i16(out, ugen_spec.special_index);
            for (i, j) in ugen_spec.input_specs {
                write_i32(out, i);
                write_i32(out, j);
            }
            for output_spec in ugen_spec.output_specs {
                write_i8(out, output_spec);
            }
        }

        write_i16(out, graph.variant_specs.len() as i16);
        for variant_spec in graph.variant_specs {
            write_pstring(out, variant_spec.name);
            for parameter_value in variant_spec.parameter_values {
                write_f32(out, parameter_value);
            }
        }
    }
}

fn write_i32(w: &mut Vec<u8>, n: i32) {
    w.write(&n.to_be_bytes()).unwrap();
}

fn write_i16(w: &mut Vec<u8>, n: i16) {
    w.write(&n.to_be_bytes()).unwrap();
}

fn write_i8(w: &mut Vec<u8>, n: i8) {
    w.write(&n.to_be_bytes()).unwrap();
}

fn write_f32(w: &mut Vec<u8>, n: f32) {
    w.write(&n.to_be_bytes()).unwrap();
}

fn write_pstring(w: &mut Vec<u8>, s: &str) {
    w.write_all(&[s.len() as u8]).unwrap();
    w.write(s.as_bytes()).unwrap();
}
