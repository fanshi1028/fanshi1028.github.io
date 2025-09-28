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
      mkDefaultPackage =
        pkgs: args:
        pkgs.haskell.packages."ghc${ghcVersion}".developPackage (
          {
            root = ./.;
          }
          // args
        );
    in
    {
      packages = nixpkgs.lib.genAttrs systems (
        system:
        let
          pkgs = import nixpkgs { inherit system overlays; };
        in
        {
          prerender = mkDefaultPackage pkgs {
            modifier =
              drv:
              pkgs.lib.pipe drv [
                (pkgs.haskell.lib.compose.setBuildTargets [ "prerender" ])
                (pkgs.haskell.lib.compose.overrideCabal (_: {
                  pname = "prerender";
                }))
              ];
          };
          fanshi1028-site-js = mkDefaultPackage pkgs.pkgsCross.ghcjs {
            modifier =
              drv:
              pkgs.lib.pipe drv [
                (pkgs.haskell.lib.compose.enableCabalFlag "production")
                (pkgs.haskell.lib.compose.setBuildTargets [ "exe:fanshi1028-site" ])
                (pkgs.haskell.lib.compose.appendConfigureFlag [ "--ghc-options=-DGHCJS_BROWSER" ])
              ];
          };
          browser_wasi_shim =
            let
              pname = "browser_wasi_shim";
              version = "0.4.2";
            in
            with pkgs;
            buildNpmPackage {
              inherit pname version;
              src = fetchFromGitHub {
                owner = "bjorn3";
                repo = pname;
                tag = "v${version}";
                hash = "sha256-okP2bT4rcqtwTk7eOdyC+DqoLACTS9srANgSEkjb06A=";
              };

              npmDepsHash = "sha256-5CUnps7UyX9U7ZRRaUy0t7lpXoOhFR8n7AEPTD0npF0=";
            };
          wasm-feature-detect =
            let
              pname = "wasm-feature-detect";
              version = "1.8.0";
            in
            with pkgs;
            buildNpmPackage {
              inherit pname version;
              src = fetchFromGitHub {
                owner = "GoogleChromeLabs";
                repo = pname;
                tag = "v${version}";
                hash = "sha256-14OZNnu6kPVfKgxa3ARnmuwgrkwpCFhK2YdK/cvW5vw=";
              };

              npmDepsHash = "sha256-ikjjc7/MZRswwNsmSJC5KqLrNKbCbKYLQ9v87t1azTc=";
            };
        }
      );

      devShells = nixpkgs.lib.genAttrs systems (
        system:
        let
          pkgs = import nixpkgs { inherit system overlays; };
        in
        {
          default = mkDefaultPackage pkgs {
            modifier =
              drv:
              pkgs.haskell.lib.addBuildTools drv (
                with pkgs;
                [
                  cabal-install
                  tailwindcss
                  ghciwatch
                  # (haskell-language-server.override { supportedGhcVersions = [ ghcVersion ]; })
                  haskell.packages."ghc${ghcVersion}".haskell-language-server
                  # NOTE: tailwindcss_4 when trying to run
                  # dyld: Symbol not found: _ubrk_clone
                  #   Referenced from: /nix/store/2dxgd64421azhmwp63h9h3hzczgvh9w7-tailwindcss_4-4.1.7/bin/.tailwindcss-wrapped (which was built for Mac OS X 13.0)
                  #   Expected in: /usr/lib/libicucore.A.dylib
                  bun
                  typescript
                  typescript-language-server
                ]
              );
            returnShellEnv = true;
          };
          wasm = pkgs.mkShell {
            name = "The miso ${system} GHC WASM ${ghcVersion} shell";
            packages =
              [
                ghc-wasm.packages.${system}.all_9_12
              ]
              ++ (with pkgs; [
                tailwindcss
                bun
              ]);
          };
        }
      );
    };
}
