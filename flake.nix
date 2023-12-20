{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.11";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = {self, nixpkgs, flake-utils} :
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hPkgs = pkgs.haskell.packages."ghc8107";
        devTools = [
          hPkgs.ghc
          hPkgs.ghcid
          hPkgs.haskell-language-server
          stack-wrapped
          pkgs.hpack
          pkgs.zlib
          pkgs.glibc
        ];
        stack-wrapped = pkgs.symlinkJoin {
          name = "stack";
          paths = [ pkgs.stack ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/stack \
              --add-flags "\
                --no-nix \
                --system-ghc \
                --no-install-ghc \
              "
          '';
        };
      in {
        formatter = pkgs.nixfmt;
        devShells.default = pkgs.mkShell {
          buildInputs = devTools;
        };
      }
    );
}
