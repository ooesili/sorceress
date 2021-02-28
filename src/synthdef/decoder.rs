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

//! A synth definition file format decoder.
//!
//! This module provides the [`decode_synthdef_file`] function which decodes a file in the [synth
//! defintiion file format] into a [data structure](). In most applications synth defintiion files
//! are encoded from in-memory data structures and decoded by the SuperCollider server, so this
//! module will not be needed. Decoding is primarily useful for converting synth defintiion files
//! into a human readable format when debugging synth defintiions.
//!
//! [synth defintiion file format]: https://doc.sccode.org/Reference/Synth-Definition-File-Format.html

use std::{
    fmt,
    io::{self, Read},
    iter::repeat_with,
};

const SYNTH_DEF_2_TYPE_ID: i32 = 0x53436766; // hex("SCgf")

/// The root of a decoded synth definition file.
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct SynthDefFile {
    /// Four byte file type id containing the ASCII characters: "SCgf".
    ///
    /// This field is not validated by the decoder.
    pub type_id: i32,

    /// The file version, currently 2.
    pub version: i32,

    /// The synth definitions contained by this file.
    pub synth_defs: Vec<SynthDef>,
}

impl SynthDefFile {
    /// Decodes a synth definition file.
    ///
    /// See [the module level documentation](self) for more.
    ///
    /// # Errors
    ///
    /// * Returns [`Error::IO`] if the function encounters an I/O error reading the input.
    /// * Returns [`Error::BadTypeID`] if the type ID field in the file header is incorrect.
    pub fn decode<R: Read>(read: R) -> Result<SynthDefFile> {
        let mut scanner = Scanner(read);
        SynthDefFile::new(&mut scanner)
    }

    fn new<R: Read>(scanner: &mut Scanner<R>) -> Result<Self> {
        let type_id = scanner.scan_i32()?;
        if type_id != SYNTH_DEF_2_TYPE_ID {
            return Err(Error::BadTypeID(type_id));
        }
        let version = scanner.scan_i32()?;
        let num_synth_defs = scanner.scan_i16()? as usize;
        let synth_defs = dotimes(num_synth_defs, || SynthDef::new(scanner))?;
        Ok(SynthDefFile {
            type_id,
            version,
            synth_defs,
        })
    }
}

/// A decoded synth definition.
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct SynthDef {
    /// The name of the synth definition.
    pub name: String,

    /// All of the constants used in the UGen graph.
    pub constants: Vec<f32>,

    /// The initial values of the parameters.
    ///
    /// These values will be used if a parameters value is not specified when creating a synth from
    /// this definition.
    pub initial_parameter_values: Vec<f32>,

    /// Names for parameters.
    ///
    /// These do not have to be one-to-one with `initial_parameter_values`. Parameters can have no
    /// name, and a single name can be used for multiple parameters.
    pub parameter_names: Vec<ParameterName>,

    /// The list of UGens that make up this synth definition.
    pub ugens: Vec<UGenSpec>,

    /// The variants of this synth definition.
    ///
    /// Variants are named alternate sets of `initial_parameter_values`. A variant can be requested
    /// when creating a new synth by appending the name of the variant to the synth definition name
    /// in the form `<name>.<variant>`.
    pub variants: Vec<VariantSpec>,
}

impl SynthDef {
    fn new<R: Read>(scanner: &mut Scanner<R>) -> Result<Self> {
        let name = scanner.scan_pstring()?;

        let num_constants = scanner.scan_i32()? as usize;
        let constants = dotimes(num_constants, || scanner.scan_f32())?;

        let num_parameters = scanner.scan_i32()? as usize;
        let initial_parameter_values = dotimes(num_parameters, || scanner.scan_f32())?;

        let num_parameter_names = scanner.scan_i32()? as usize;
        let parameter_names = dotimes(num_parameter_names, || {
            Ok(ParameterName {
                value: scanner.scan_pstring()?,
                parameter_index: scanner.scan_i32()? as usize,
            })
        })?;

        let num_ugens = scanner.scan_i32()? as usize;
        let ugens = dotimes(num_ugens, || UGenSpec::new(scanner))?;

        let num_variants = scanner.scan_i16()? as usize;
        let variants = dotimes(num_variants, || {
            Ok(VariantSpec {
                name: scanner.scan_pstring()?,
                parameters: dotimes(num_parameters, || scanner.scan_f32())?,
            })
        })?;

        Ok(SynthDef {
            name,
            constants,
            initial_parameter_values,
            parameter_names,
            ugens,
            variants,
        })
    }
}

/// A named parameter.
///
/// Parameter names allow controls to be referenced by name in adition to their index.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ParameterName {
    /// The name of the parameter.
    pub value: String,

    /// The index of the parameter this name refers to.
    pub parameter_index: usize,
}

/// The specification of a unit generator.
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct UGenSpec {
    /// The class name of the unit generator.
    ///
    /// Examples include `SinOsc`, `Out`, and `Control`.
    pub class_name: String,

    /// The rate at which the UGen computes values.
    ///
    /// There are three rates numbered 0, 1, 2 as follows:

    /// * `0 = scalar rate` - one sample is computed at initialization time only.
    /// * `1 = control rate` - one sample is computed each control period.
    /// * `2 = audio rate` - one sample is computed for each sample of audio output.
    pub rate: i8,

    /// This value is used by some unit generators for a special purpose.
    ///
    /// For example, UnaryOpUGen and BinaryOpUGen use it to indicate which operator to perform. If
    /// not used it should be set to zero.
    pub special_index: i16,

    /// The inputs to this unit generator.
    pub inputs: Vec<Input>,

    /// The list of outputs of this unit generator.
    ///
    /// Each element in the vector is the `rate` of the output, using the same number as the `rate`
    /// field of this struct.
    pub outputs: Vec<i8>,
}

impl UGenSpec {
    fn new<R: Read>(scanner: &mut Scanner<R>) -> Result<Self> {
        let class_name = scanner.scan_pstring()?;
        let rate = scanner.scan_i8()?;
        let num_inputs = scanner.scan_i32()? as usize;
        let num_outputs = scanner.scan_i32()? as usize;
        let special_index = scanner.scan_i16()?;
        let inputs = dotimes(num_inputs, || {
            let x = scanner.scan_i32()?;
            let y = scanner.scan_i32()? as usize;
            Ok(if x == -1 {
                Input::Constant { index: y }
            } else {
                Input::UGen {
                    index: x as usize,
                    output_index: y,
                }
            })
        })?;
        let outputs = dotimes(num_outputs, || scanner.scan_i8())?;

        Ok(UGenSpec {
            class_name,
            rate,
            special_index,
            inputs,
            outputs,
        })
    }
}

/// An input to a UGen.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Input {
    /// An input that refers to the output of another UGen.
    UGen { index: usize, output_index: usize },

    /// An input that refers to a constant value.
    Constant { index: usize },
}

/// An alternate set of default parameters for a synth definition.
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct VariantSpec {
    /// The name of the variant.
    pub name: String,

    /// The initial parameter values.
    pub parameters: Vec<f32>,
}

fn dotimes<T, F>(len: usize, func: F) -> Result<Vec<T>>
where
    F: FnMut() -> Result<T>,
{
    repeat_with(func).take(len).collect::<Result<_>>()
}

struct Scanner<R: Read>(R);

impl<R: Read> Scanner<R> {
    fn scan_i8(&mut self) -> Result<i8> {
        let mut buf: [u8; 1] = [0; 1];
        self.0.read_exact(&mut buf)?;
        Ok(i8::from_be_bytes(buf))
    }

    fn scan_i16(&mut self) -> Result<i16> {
        let mut buf: [u8; 2] = [0; 2];
        self.0.read_exact(&mut buf)?;
        Ok(i16::from_be_bytes(buf))
    }

    fn scan_i32(&mut self) -> Result<i32> {
        let mut buf: [u8; 4] = [0; 4];
        self.0.read_exact(&mut buf)?;
        Ok(i32::from_be_bytes(buf))
    }

    fn scan_f32(&mut self) -> Result<f32> {
        let mut buf: [u8; 4] = [0; 4];
        self.0.read_exact(&mut buf)?;
        Ok(f32::from_be_bytes(buf))
    }

    fn scan_pstring(&mut self) -> Result<String> {
        let mut len_buf: [u8; 1] = [0; 1];
        self.0.read_exact(&mut len_buf)?;
        let len = len_buf[0] as usize;
        let mut buf = vec![0; len];
        self.0.read_exact(&mut buf)?;
        Ok(String::from_utf8(buf).unwrap())
    }
}

type Result<T> = std::result::Result<T, Error>;

/// The error type of decoding operations.
#[derive(Debug)]
pub enum Error {
    /// An error indiciating that the type ID field of the synth definition file was invalid.
    ///
    /// The value inside the variant is the unexpected type ID that was read.
    BadTypeID(i32),

    /// An I/O error that occurred wile decoding the synth definition.
    IO(io::Error),
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Error {
        Error::IO(err)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::BadTypeID(type_id) => write!(
                f,
                "bad type ID in header: expected {:x}, got {:x}",
                SYNTH_DEF_2_TYPE_ID, type_id
            ),
            Error::IO(err) => write!(f, "i/o error: {}", err),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Error::BadTypeID(_) => None,
            Error::IO(err) => Some(err),
        }
    }
}
