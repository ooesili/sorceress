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

use super::{Rate, Scalar, SynthDef, UGenSpec};
use std::io;
use std::sync::Arc;

/// Encodes a set of synth definitions into the [SynthDef2 file format].
///
/// Synth definition files are read by the SuperCollider server and define collections of unit
/// generators and their connections.
///
/// [SynthDef2 file format]: https://doc.sccode.org/Reference/Synth-Definition-File-Format.html
///
/// # Examples:
///
/// ```
/// use sorceress::{
///     synthdef::{encoder::encode_synth_defs, SynthDef, Value},
///     ugen::{Out, SinOsc},
/// };
///
/// let synth_def = SynthDef::new("example", Out::ar().channels(SinOsc::ar().freq(440)));
/// let encoded_file = encode_synth_defs(vec![synth_def])?;
/// # std::io::Result::Ok(())
/// ```
pub fn encode_synth_defs(synth_defs: impl IntoIterator<Item = SynthDef>) -> io::Result<Vec<u8>> {
    let expanded_synths = synth_defs
        .into_iter()
        .map(|synthdef| {
            (
                synthdef.name,
                synthdef.graph.into_iter().collect::<Vec<_>>(),
            )
        })
        .collect::<Vec<_>>();
    let graphs = expanded_synths
        .iter()
        .map(|(name, expanded_inputs)| SynthDefGraph::new(&name, expanded_inputs))
        .collect();

    let mut buffer = Vec::new();
    encode_graphs(graphs, &mut buffer)?;

    Ok(buffer)
}

struct SynthDefGraph<'a> {
    name: &'a str,
    constants: Vec<Const>,
    parameter_specs: Vec<ParameterSpec<'a>>,
    ugens: Vec<SynthDefUgen<'a>>,
    variant_specs: Vec<VariantSpec<'a>>,
    control_spec: Arc<UGenSpec<Scalar>>,
}

impl<'a> SynthDefGraph<'a> {
    fn new(name: &'a str, inputs: &'a [Scalar]) -> Self {
        let mut file = SynthDefGraph {
            name,
            constants: Vec::new(),
            parameter_specs: Vec::new(),
            ugens: Vec::new(),
            variant_specs: Vec::new(),
            control_spec: Arc::new(UGenSpec {
                name: "Control".to_owned(),
                rate: Rate::Audio,
                special_index: 0,
                inputs: vec![],
                outputs: vec![Rate::Control],
            }),
        };
        for value in inputs.iter() {
            file.encode(value);
        }
        file
    }

    fn encode(&mut self, input: &'a Scalar) -> Vec<(i32, i32)> {
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
                vec![(-1, index as i32)]
            }
            Scalar::Parameter(ref parameter) => {
                // First see if the control ugen exists. If it does not, add it with a single
                // output. If it does exist, append a control rate output to the ugen. Add an entry
                // to the parameter_names and initial_parameter_values vectors.

                let control_ugen_index = self
                    .ugens
                    .iter()
                    .position(|u| u.source.name == "Control")
                    .unwrap_or_else(|| {
                        let ugen_spec = UgenSpecRef {
                            name: "Control",
                            rate: Rate::Control.into(),
                            special_index: 0,
                            input_specs: vec![],
                            output_specs: vec![],
                        };
                        self.ugens.push(SynthDefUgen {
                            index: self.ugens.len(),
                            source: self.control_spec.clone(),
                            spec: ugen_spec,
                        });
                        self.ugens.len() - 1
                    });

                let parameter_spec = ParameterSpec {
                    name: &parameter.name,
                    initial_value: parameter.initial_value,
                };
                let parameter_index = self
                    .parameter_specs
                    .iter()
                    .position(|p| p == &parameter_spec)
                    .unwrap_or_else(|| {
                        self.ugens[control_ugen_index]
                            .spec
                            .output_specs
                            .push(Rate::Control.into());
                        self.parameter_specs.push(parameter_spec);
                        self.parameter_specs.len() - 1
                    });

                vec![(control_ugen_index as i32, parameter_index as i32)]
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
                            // TODO: this is wrong
                            .map(|input| self.encode(input)[0])
                            .collect();
                        self.ugens.push(SynthDefUgen {
                            index: self.ugens.len(),
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
                vec![(index as i32, *output_index)]
            }
        }
    }
}

#[derive(PartialEq)]
struct Const(f32);

#[derive(Debug, PartialEq)]
struct ParameterSpec<'a> {
    name: &'a str,
    initial_value: f32,
}

#[derive(Debug)]
struct SynthDefUgen<'a> {
    index: usize,
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

fn encode_graphs<W: io::Write>(graphs: Vec<SynthDefGraph>, out: &mut W) -> io::Result<()>
where {
    let header: i32 = 0x53436766; // hex("SCgf")
    write_i32(out, header)?;

    let file_version: i32 = 2;
    write_i32(out, file_version)?;

    write_i16(out, graphs.len() as i16)?;
    for graph in graphs {
        write_pstring(out, graph.name)?;
        write_i32(out, graph.constants.len() as i32)?;
        for constant in graph.constants {
            write_f32(out, constant.0)?;
        }

        write_i32(out, graph.parameter_specs.len() as i32)?;
        for parameter_spec in graph.parameter_specs.iter() {
            write_f32(out, parameter_spec.initial_value)?;
        }

        write_i32(out, graph.parameter_specs.len() as i32)?;
        for (position, parameter_spec) in graph.parameter_specs.iter().enumerate() {
            write_pstring(out, &parameter_spec.name)?;
            write_i32(out, position as i32)?;
        }

        write_i32(out, graph.ugens.len() as i32)?;
        for ugen_spec in graph.ugens.into_iter().map(|u| u.spec) {
            write_pstring(out, ugen_spec.name)?;
            write_i8(out, ugen_spec.rate)?;
            write_i32(out, ugen_spec.input_specs.len() as i32)?;
            write_i32(out, ugen_spec.output_specs.len() as i32)?;
            write_i16(out, ugen_spec.special_index)?;
            for (i, j) in ugen_spec.input_specs {
                write_i32(out, i)?;
                write_i32(out, j)?;
            }
            for output_spec in ugen_spec.output_specs {
                write_i8(out, output_spec)?;
            }
        }

        write_i16(out, graph.variant_specs.len() as i16)?;
        for variant_spec in graph.variant_specs {
            write_pstring(out, variant_spec.name)?;
            for parameter_value in variant_spec.parameter_values {
                write_f32(out, parameter_value)?;
            }
        }
    }

    Ok(())
}

fn write_i32<W: io::Write>(w: &mut W, n: i32) -> io::Result<usize> {
    w.write(&n.to_be_bytes())
}

fn write_i16<W: io::Write>(w: &mut W, n: i16) -> io::Result<usize> {
    w.write(&n.to_be_bytes())
}

fn write_i8<W: io::Write>(w: &mut W, n: i8) -> io::Result<usize> {
    w.write(&n.to_be_bytes())
}

fn write_f32<W: io::Write>(w: &mut W, n: f32) -> io::Result<usize> {
    w.write(&n.to_be_bytes())
}

fn write_pstring<W: io::Write>(w: &mut W, s: &str) -> io::Result<usize> {
    w.write_all(&[s.len() as u8])?;
    w.write(s.as_bytes())
}
