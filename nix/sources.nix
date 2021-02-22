let
  # This controls the version of nixpkgs used for builds and nix-shell. The
  # unstable-small nixpkgs channel is used to get the latest version of Rust.
  #
  # Find the latest version:
  # https://channels.nixos.org/nixos-unstable-small/git-revision
  version = "19d715c5734ead0c605d0f6baabd46b4851e7f25";

in import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/${version}.tar.gz")
