let overlay = self: super: {
  supercollider = self.callPackage ./nix/supercollider.nix {};
};

in { pkgs ? import ./nix/sources.nix { overlays = [ overlay ]; } }:

pkgs.rustPlatform.buildRustPackage {
  pname = "sorceress";
  version = "0.0.1";

  cargoSha256 = "1ym0d1v58mhcbv505qifiyksmbl8d9pl3q7r1s171qnpgsnfvr9b";
  nativeBuildInputs = [ pkgs.supercollider ];
  src = pkgs.lib.cleanSource ./.;
}
