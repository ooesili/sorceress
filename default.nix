let overlay = self: super: {
  supercollider = self.callPackage ./nix/supercollider.nix {};
};

in { pkgs ? import ./nix/sources.nix { overlays = [ overlay ]; } }:

pkgs.rustPlatform.buildRustPackage {
  name = "sorceress";

  cargoSha256 = "00m2qrdd3c21d6w94jfhvia55nkh9cjnvw1ajg5b6kw7ynrzy79d";
  nativeBuildInputs = [ pkgs.supercollider ];
  src = pkgs.lib.cleanSource ./.;
}
