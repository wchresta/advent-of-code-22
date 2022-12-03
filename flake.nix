{
  description = "Advent of Code in Haskell";
  inputs.nixpkgs.url = "nixpkgs";
  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        #overlays = [ self.overlay ];
      });
    in
    {
      #overlay = (final: prev: {
        #adventofcode = final.haskellPackages.callCabal2nix "adventofcode" ./. {};
      #});
      #packages = forAllSystems (system: {
         #haskell-hello = nixpkgsFor.${system}.haskell-hello;
      #});
      #defaultPackage = forAllSystems (system: self.packages.${system}.haskell-hello);
      #checks = self.packages;
      devShell = forAllSystems (system:
        let
          pkgs = nixpkgsFor.${system};
        in pkgs.mkShell {
          buildInputs = [
            (pkgs.haskellPackages.ghcWithPackages (p: [ p.split ]))
          ];
        });
  };
}
