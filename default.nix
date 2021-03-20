{ pkgs ? import ./nix/sources.nix {}
, supercollider ? pkgs.callPackage ./nix/supercollider.nix {}
}:

pkgs.rustPlatform.buildRustPackage {
  pname = "sorceress";
  version = "0.0.1";

  cargoSha256 = "1ym0d1v58mhcbv505qifiyksmbl8d9pl3q7r1s171qnpgsnfvr9b";
  nativeBuildInputs = [ supercollider ];
  src = pkgs.lib.cleanSource ./.;
}
