let overlay = self: super: {
  supercollider = self.callPackage ./nix/supercollider.nix {};
};

in { pkgs ? import ./nix/sources.nix { overlays = [ overlay ]; } }:

pkgs.rustPlatform.buildRustPackage {
  name = "sorceress";

  cargoSha256 = "0zvvcmzxywiwkqdndsvjla4f1phnd977f1r12qqk8p55bb5bzn0q";
  nativeBuildInputs = [ pkgs.supercollider ];
  src = pkgs.lib.cleanSource ./.;
}
