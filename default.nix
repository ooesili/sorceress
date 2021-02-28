{ pkgs ? import ./nix/sources.nix {}
, supercollider ? pkgs.callPackage ./nix/supercollider.nix {}
}:

pkgs.rustPlatform.buildRustPackage {
  pname = "sorceress";
  version = "0.0.1";

  cargoSha256 = "11adq20mcqf02mw7b5lflydh9mpj1y6a4ifqdafpidrcczdjw0bs";
  nativeBuildInputs = [ supercollider ];
  src = pkgs.lib.cleanSource ./.;
}
