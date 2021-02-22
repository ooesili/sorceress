use pretty_assertions::assert_eq;
use sorceress::{
    synthdef::{decoder::decode, encoder::encode_synth_defs, SynthDef},
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

    test_synthdefs.push(SynthDef::new(
        "SinOsc-mono",
        ugen::Out::ar()
            .bus(0)
            .channels(ugen::SinOsc::ar().freq(440)),
    ));

    test_synthdefs.push(SynthDef::new(
        "SinOsc-stereo-expanded",
        ugen::Out::ar()
            .bus(0)
            .channels(ugen::SinOsc::ar().freq(vec![440, 220])),
    ));

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
        let encoded_synthdef = encode_synth_defs(iter::once(synthdef)).unwrap();

        let fixture_synthdef = fixture_synthdefs
            .get(&name)
            .ok_or_else(|| format!("no fixture synthdef named {}", name))
            .unwrap();

        compare(&encoded_synthdef, fixture_synthdef);
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

fn compare(lhs: &[u8], rhs: &[u8]) {
    let format = |synthdef: &[u8]| denorm::SynthDefFile::new(&decode(synthdef).unwrap());
    assert_eq!(format(&lhs), format(&rhs));
}

fn temp_dir() -> TempDir {
    TempDir::new("sorceress-tests").unwrap()
}

mod denorm {
    //! A denormalized verison synth definition with more useful equality comparisons.

    use sorceress::synthdef::decoder;
    use std::collections::HashMap;

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
        ugens: Vec<UGenSpec>,
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

    #[derive(Debug, PartialEq)]
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
                            index,
                            output_index,
                        },
                        decoder::Input::Constant { index } => {
                            Input::Constant(synthdef.constants[index])
                        }
                    })
                    .collect(),
                outputs: ugen.outputs.clone(),
            }
        }
    }

    #[derive(Debug, PartialEq)]
    enum Input {
        UGen { index: usize, output_index: usize },
        Constant(f32),
    }

    #[derive(Debug, PartialEq)]
    struct VariantSpec {
        parameters: Vec<f32>,
    }
}
