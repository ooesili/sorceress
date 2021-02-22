# Sorceress

[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org) [![Crates.io](https://img.shields.io/crates/v/sorceress.svg?style=flat-square)](https://crates.io/crates/sorceress) [![docs.rs docs](https://img.shields.io/badge/docs-latest-blue.svg?style=flat-square)](https://docs.rs/sorceress) [![Gitter](https://badges.gitter.im/sorceress-rs/community.svg)](https://gitter.im/sorceress-rs/community?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

A Rust environment for sound synthesis and algorithmic composition.

![Sorceress](/assets/sorceress.svg)

## Overview

Sorceress is a Rust crate that provides a creative coding environment for:

* **Sound synthesis** - build audio synthesizers by connecting *unit generators* together into signal graphs. SuperCollider provides [hundreds of unit generators](https://doc.sccode.org/Browse.html#UGens) to choose from including things like wave generators, noise generators, filters, envelopes, compressors, resonators, physical simulations, Fourier transforms, and much more.

* **Algorithmic composition** - write code to create music, anywhere from using code as a musical notation system to full-fledged generative composition where large scale structures of a music piece are determined by computational algorithms.

### Why SuperCollider?

SuperCollider is a powerful and mature platform for audio synthesis with decades of development effort behind it. SuperCollider's [Client and Server](https://doc.sccode.org/Guides/ClientVsServer.html) architecture lets us to leverage all of the features offered by SuperCollider's audio synthesis server, from Rust:

* A real-time audio synthesis engine
* A massive library of unit generators
* Audio I/O with your operation system and sound card

### Why Rust?

There are projects in many other programming languages for interacting with SuperCollider including [Overtone](https://overtone.github.io/), [Tidal](https://tidalcycles.org/), and [Sonic Pi](https://sonic-pi.net/). I really like programming in Rust and I could not find any such project using Rust so I started building Sorceress.

## Example

This example plays a sine wave at 220 Hz for 1 second:

```rust
use anyhow::Result;
use sorceress::{
    server::{self, Server},
    synthdef::{encoder::encode_synth_defs, SynthDef},
    ugen,
};
use std::{thread::sleep, time::Duration};

fn main() -> Result<()> {
    let server = Server::new("127.0.0.1:57110")?;

    let sine_wave = SynthDef::connect(
        "sine_wave",
        ugen::Out::ar().channels(ugen::Pan2::ar().input(ugen::SinOsc::ar().freq(220))),
    );
    let encoded_synthdef = encode_synth_defs(vec![sine_wave])?;
    server.send_sync(server::SynthDefRecv::new(&encoded_synthdef))?;

    server.send(server::SynthNew::new("sine_wave", 1, vec![]))?;
    sleep(Duration::from_secs(1));

    server.reset()?;

    Ok(())
}
```

## Setup

With [cargo-edit](https://github.com/killercup/cargo-edit) installed run:

```
$ cargo add sorceress
```

You must [install SuperCollider](https://supercollider.github.io/download) separately from the `sorceress` crate. 

**Note:** Sorceress does not run SuperCollider for you at this time, so you must boot a server yourself. The recommended way to do this by starting the server in **scide**, SuperCollider's built in IDE.

## Documentation

The primary source of documentation for Sorceress is the [crate documentation on docs.rs](https://docs.rs/sorceress).

## Contributing

See [CONTRIBUTING](CONTRIBUTING.md) for details on creating issues or making pull requests.

## License

Sorceress is free software available under Version 3 the GNU General Public License. See [COPYING](COPYING) for details.
