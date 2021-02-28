use pretty_assertions::assert_eq;
use sorceress::{
    synthdef::{decoder, encoder::encode_synth_defs, Input, SynthDef},
    ugen,
};
use std::{
    collections::HashMap,
    fs,
    io::{self, Write},
    iter,
    process::Command,
};
use tempdir::TempDir;

#[test]
fn test_synthdefs() {
    let mut test_synthdefs = Vec::new();

    test_synthdefs.push(SynthDef::new("SinOsc-mono", |_| {
        ugen::Out::ar()
            .bus(0)
            .channels(ugen::SinOsc::ar().freq(440))
    }));

    test_synthdefs.push(SynthDef::new("SinOsc-stereo-expanded", |_| {
        ugen::Out::ar()
            .bus(0)
            .channels(ugen::SinOsc::ar().freq(vec![440, 220]))
    }));

    test_synthdefs.push(SynthDef::new("bubbles", |params| {
        let out = params.named("out", 0.0);
        let glissando_function = ugen::LFSaw::kr()
            .freq(0.4)
            .madd(24, ugen::LFSaw::kr().freq(vec![8.0, 7.23]).madd(3, 80))
            .midicps();
        let echoing_sine_wave = ugen::CombN::ar()
            .input(ugen::SinOsc::ar().freq(glissando_function).mul(0.04))
            .decay_time(4);
        ugen::Out::ar().bus(out).channels(echoing_sine_wave)
    }));

    test_synthdefs.push(SynthDef::new("diskout", |params| {
        let buffer_number = params.named("bufnum", 0.0);
        ugen::DiskOut::ar()
            .bufnum(buffer_number)
            .channels(ugen::In::ar().bus(0).number_of_channels(2))
    }));

    verify_synthdefs_against_fixtures(test_synthdefs)
}

fn verify_synthdefs_against_fixtures(test_synthdefs: Vec<SynthDef>) {
    let test_synthdefs = test_synthdefs
        .into_iter()
        .map(|synthdef| (synthdef.name().to_owned(), synthdef))
        .collect::<HashMap<_, _>>();

    let fixture_synthdefs = load_fixture_synthdefs();

    for name in fixture_synthdefs.keys() {
        if !test_synthdefs.contains_key(name) {
            panic!("no test found for fixture synthdef named {}", name);
        }
    }

    for (name, synthdef) in test_synthdefs.into_iter() {
        let encoded_synthdef = encode_synth_defs(iter::once(synthdef));

        let fixture_synthdef = fixture_synthdefs
            .get(&name)
            .ok_or_else(|| format!("no fixture synthdef named {}", name))
            .unwrap();

        compare(fixture_synthdef, &encoded_synthdef, &name);
    }
}

fn load_fixture_synthdefs() -> HashMap<String, Vec<u8>> {
    let fake_home_dir = temp_dir();
    let synthdefs_dir = temp_dir();
    let output = Command::new("sclang")
        .arg("tests/data/synthdefs.sc")
        .env("SYNTHDEFS_DIR", synthdefs_dir.as_ref())
        // HOME is set to `/homeless-shelter` in nix builds, so this fixes it.
        .env("HOME", fake_home_dir.as_ref())
        .output()
        .unwrap();

    if !output.status.success() {
        println!("========== sclang output ==========");
        io::stdout().write_all(&output.stdout).unwrap();
        io::stdout().write_all(&output.stderr).unwrap();
        panic!("failed to encode fixture synthdefs with sclang");
    }

    fs::read_dir(&synthdefs_dir)
        .unwrap()
        .map(|file| {
            let path = file.unwrap().path();
            let synthdef_name = path.file_stem().unwrap().to_str().unwrap().to_owned();
            let encoded_synthdef = fs::read(&path).unwrap();
            (synthdef_name, encoded_synthdef)
        })
        .collect::<HashMap<_, _>>()
}

fn compare(lhs: &[u8], rhs: &[u8], name: &str) {
    let format = |synthdef: &[u8]| {
        denorm::SynthDefFile::new(&decoder::SynthDefFile::decode(synthdef).unwrap())
    };
    assert_eq!(
        format(&lhs),
        format(&rhs),
        "comparing synth definitions for synthdef: {}",
        name
    );
}

fn temp_dir() -> TempDir {
    TempDir::new("sorceress-tests").unwrap()
}

mod denorm {
    //! A denormalized verison synth definition with more useful equality comparisons.

    use sorceress::synthdef::decoder;
    use std::{
        collections::{HashMap, HashSet},
        hash::Hash,
    };

    impl SynthDefFile {
        pub fn new(file: &decoder::SynthDefFile) -> SynthDefFile {
            SynthDefFile {
                synth_defs: file.synth_defs.iter().map(SynthDef::new).collect(),
            }
        }
    }

    #[derive(Debug, PartialEq)]
    pub struct SynthDefFile {
        synth_defs: Vec<SynthDef>,
    }

    #[derive(Debug, PartialEq)]
    struct SynthDef {
        name: String,
        parameters: HashMap<String, usize>,
        ugens: HashSet<UGenSpec>,
        variants: HashMap<String, VariantSpec>,
    }

    impl SynthDef {
        fn new(synthdef: &decoder::SynthDef) -> SynthDef {
            let name = synthdef.name.clone();
            let parameters = synthdef
                .parameter_names
                .iter()
                .map(|parameter_name| {
                    (parameter_name.value.clone(), parameter_name.parameter_index)
                })
                .collect();
            let ugens = synthdef
                .ugens
                .iter()
                .map(|ugen| UGenSpec::new(synthdef, ugen))
                .collect();
            SynthDef {
                name,
                parameters,
                ugens,
                variants: synthdef
                    .variants
                    .iter()
                    .map(|variant| {
                        (
                            variant.name.clone(),
                            VariantSpec {
                                parameters: variant.parameters.clone(),
                            },
                        )
                    })
                    .collect(),
            }
        }
    }

    #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
    struct UGenSpec {
        class_name: String,
        rate: i8,
        special_index: i16,
        inputs: Vec<Input>,
        outputs: Vec<i8>,
    }

    impl UGenSpec {
        fn new(synthdef: &decoder::SynthDef, ugen: &decoder::UGenSpec) -> UGenSpec {
            UGenSpec {
                class_name: ugen.class_name.clone(),
                rate: ugen.rate,
                special_index: ugen.special_index,
                inputs: ugen
                    .inputs
                    .iter()
                    .map(|input| match *input {
                        decoder::Input::UGen {
                            index,
                            output_index,
                        } => Input::UGen {
                            spec: UGenSpec::new(synthdef, &synthdef.ugens[index]),
                            output_index,
                        },
                        decoder::Input::Constant { index } => {
                            Input::Constant(synthdef.constants[index].to_string())
                        }
                    })
                    .collect(),
                outputs: ugen.outputs.clone(),
            }
        }
    }

    #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
    enum Input {
        UGen { spec: UGenSpec, output_index: usize },
        Constant(String),
    }

    #[derive(Debug, PartialEq)]
    struct VariantSpec {
        parameters: Vec<f32>,
    }
}
