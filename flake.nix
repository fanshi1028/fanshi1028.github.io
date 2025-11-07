{

  inputs = {
    miso = {
      url = "github:dmjio/miso";
      flake = false;
    };
    # copied from miso's flake
    nixpkgs.url = "github:nixos/nixpkgs?rev=9e2e8a7878573d312db421d69e071690ec34e98c";
    ghc-wasm.url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";
  };

  outputs =
    {
      self,
      miso,
      nixpkgs,
      ghc-wasm,
    }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      ghcVersion = "9122";
      overlays = [ (import "${miso}/nix/overlay.nix") ];
    in
    {

      devShells = nixpkgs.lib.genAttrs systems (
        system:
        let
          pkgs = import nixpkgs { inherit system overlays; };
        in
        {
          default = pkgs.haskell.packages."ghc${ghcVersion}".developPackage {
            root = ./.;
            modifier =
              drv:
              pkgs.haskell.lib.addBuildTools drv (
                with pkgs;
                [
                  ghciwatch
                  cabal-install
                  # (haskell-language-server.override { supportedGhcVersions = [ ghcVersion ]; })
                  haskell.packages."ghc${ghcVersion}".haskell-language-server
                  # NOTE: tailwindcss_4 when trying to run
                  # dyld: Symbol not found: _ubrk_clone
                  #   Referenced from: /nix/store/2dxgd64421azhmwp63h9h3hzczgvh9w7-tailwindcss_4-4.1.7/bin/.tailwindcss-wrapped (which was built for Mac OS X 13.0)
                  #   Expected in: /usr/lib/libicucore.A.dylib
                  tailwindcss
                  bun
                ]
              );
            returnShellEnv = true;
          };
          wasm = pkgs.mkShell {
            name = "The miso ${system} GHC WASM ${ghcVersion} shell";
            packages = [
              ghc-wasm.packages.${system}.all_9_12
              pkgs.http-server
            ];
          };
        }
      );
    };
}
