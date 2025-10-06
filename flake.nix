{
  nixConfig.extra-substituters = [
    "https://fanshi1028-personal.cachix.org"
  ];

  inputs = {
    miso = {
      url = "github:dmjio/miso";
      flake = false;
    };
    # copied from miso's flake
    nixpkgs.url = "github:nixos/nixpkgs?rev=9e2e8a7878573d312db421d69e071690ec34e98c";
    ghc-wasm.url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";
    browser_wasi_shim = {
      url = "github:bjorn3/browser_wasi_shim?ref=v0.4.2";
      flake = false;
    };
    wasm-feature-detect = {
      url = "github:GoogleChromeLabs/wasm-feature-detect?ref=v1.8.0";
      flake = false;
    };
  };

  outputs =
    {
      self,
      miso,
      nixpkgs,
      ghc-wasm,
      browser_wasi_shim,
      wasm-feature-detect,
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
      mkHaskellPackages =
        pkgs:
        pkgs.haskell.packages."ghc${ghcVersion}".override {
          overrides = hself: hsuper: {
            hie-bios = hself.callHackageDirect {
              pkg = "hie-bios";
              ver = "0.17.0";
              sha256 = "sha256-0acJbwE1V7yid7UY1fzzA4U91ouAqm0ftHb+ceRk8zU=";
            } { };
            hiedb = hself.callHackageDirect {
              pkg = "hiedb";
              ver = "0.7.0.0";
              sha256 = "sha256-lpaJQsMP4OxEQix0gXlzpTdDJ8yspZwOeL89ysmXClY=";
            } { };
            ghcide = hself.callHackageDirect {
              pkg = "ghcide";
              ver = "2.12.0.0";
              sha256 = "sha256-FJkpM5tGK7qA0FzXLB8zdo4NiCNdpjtlvZzxSWNCClQ";
            } { };
            hls-graph = hself.callHackageDirect {
              pkg = "hls-graph";
              ver = "2.12.0.0";
              sha256 = "sha256-tCDF2YPdjEfr7SWcCe/zcH2LwuMI+8FurHYG4v3ig/w=";
            } { };
            hls-plugin-api = hself.callHackageDirect {
              pkg = "hls-plugin-api";
              ver = "2.12.0.0";
              sha256 = "sha256-IYcegXWZh9b4GythKHqn8mcvJFwz8dYhzcI1+7dnI+g=";
            } { };
            hls-test-utils = hself.callHackageDirect {
              pkg = "hls-test-utils";
              ver = "2.12.0.0";
              sha256 = "sha256-L5ZC7V1zG9Q6liLTHRhACkazUUCAY+dGcnFypFVdCnM=";
            } { };
            haskell-language-server = pkgs.lib.pipe hsuper.haskell-language-server (
              with pkgs.haskell.lib.compose;
              [
                (overrideCabal {
                  version = "2.12.0.0";
                  sha256 = "sha256-F2mzrBp+wJcdD+xKh1XVJPf3JN/I76zk9ZD9q26dR9E=";
                })
                (appendBuildFlags [
                  "-f-floskell"
                  "-f-stylishhaskell"
                  "-f-fourmolu"
                  "-f-cabalfmt"
                ])
              ]
            );
          };
        };
      mkDefaultPackage =
        pkgs: args:
        (mkHaskellPackages pkgs).developPackage (
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
          inherit (pkgs) tailwindcss closurecompiler;
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
              ];
          };
          browser_wasi_shim = pkgs.buildNpmPackage {
            pname = "browser_wasi_shim";
            version = "0.4.2";
            src = browser_wasi_shim;
            npmDepsHash = "sha256-5CUnps7UyX9U7ZRRaUy0t7lpXoOhFR8n7AEPTD0npF0=";
          };
          wasm-feature-detect = pkgs.buildNpmPackage {
            pname = "wasm-feature-detect";
            version = "1.8.0";
            src = wasm-feature-detect;
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
                  ((mkHaskellPackages pkgs).haskell-language-server)
                  # NOTE: tailwindcss_4 when trying to run
                  # dyld: Symbol not found: _ubrk_clone
                  #   Referenced from: /nix/store/2dxgd64421azhmwp63h9h3hzczgvh9w7-tailwindcss_4-4.1.7/bin/.tailwindcss-wrapped (which was built for Mac OS X 13.0)
                  #   Expected in: /usr/lib/libicucore.A.dylib
                  bun
                  typescript
                  typescript-language-server
                  prettier
                  closurecompiler
                  # webpack-cli
                  # swc
                  uglify-js
                  http-server
                ]
              );
            returnShellEnv = true;
          };
          wasm = pkgs.mkShell {
            name = "The miso ${system} GHC WASM ${ghcVersion} shell";
            packages = [ ghc-wasm.packages.${system}.all_9_12 ];
          };
        }
      );
    };
}
