let
  # This controls the version of nixpkgs used for builds and nix-shell. The
  # unstable-small nixpkgs channel is used to get the latest version of Rust.
  #
  # Find the latest version:
  # https://channels.nixos.org/nixos-unstable-small/git-revision
  version = "d7bae4c43875b0a62b3f2220f937117976568390";

in import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/${version}.tar.gz")
