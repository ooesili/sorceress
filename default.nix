{ pkgs ? import ./nix/sources.nix {}
, supercollider ? pkgs.callPackage ./nix/supercollider.nix {}
}:

pkgs.rustPlatform.buildRustPackage {
  pname = "sorceress";
  version = "0.0.1";

  cargoSha256 = "0ii2hk4nvfsxn9ws8836jdhjpx1j9gmlmdsbgvp7p2bn4hiacdjm";
  nativeBuildInputs = [ supercollider ];
  src = pkgs.lib.cleanSource ./.;
}
