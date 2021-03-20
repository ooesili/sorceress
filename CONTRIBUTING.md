# Contributing

## Testing

Sorceress is built and tested using [Nix](https://nixos.org/guides/how-nix-works.html). Nix is not strictly required to build or use sorceress, but it does enable you to run the test suite in a reproducible environment.

Install Nix:

```
$ curl -L https://nixos.org/nix/install | sh
```

Build and test sorceress:

```
$ nix-build
```

Run a development environment with all dependencies present, including Rust and SuperCollider:

```
$ nix-shell
```

Now you should be able to run `cargo test`, `cargo build`, etc.

## Issues

If you are filing a bug report please use the following template.

```markdown
# What I Did

Explain what you were doing when you saw the unexpected behavior including steps to reproduce the issue.

# What I Expected

Explain what you expected.

# What I Saw

Explain what you saw that you did not expect.
```

## Pull Request

If you would like to make a pull request please do the following:

* If you are proposing a substantial change, please open an issue first to make nobody else is already working on it and that it is in line with the project's direction.

* Include tests for all new features and bug fixes, within reason. If you don't know how to test something feel free to open an issue or ask in [Gitter](https://gitter.im/sorceress-rs).

* Make sure the code in the README compiles. There is unfortunately no automation for this yet.

* Make sure if you've included a copy of GPL license notice at the top of all new source code files.
  ```
  Sorceress
  Copyright (C) 2021  Wesley Merkel

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <https://www.gnu.org/licenses/>.
  ```

* Add a description of any user-facing changes in the _Unreleased_ section of `CHANGELOG.md` following the format described by the [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) project.
