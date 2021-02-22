{ pkgs ? import ./nix/sources.nix {}
, supercollider ? pkgs.callPackage ./nix/supercollider.nix {}
}:

pkgs.rustPlatform.buildRustPackage {
  pname = "sorceress";
  version = "0.0.1";

  cargoSha256 = "1yipzyrw1ww4yadrxn7xb36slz92baamcsb541qvlnbpavld57kh";
  nativeBuildInputs = [ supercollider ];
  src = pkgs.lib.cleanSource ./.;
}
