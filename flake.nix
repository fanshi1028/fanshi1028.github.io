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
      make-haskell-language-server-scope = pkgs: hself: hsuper: {
        # NOTE: https://github.com/NixOS/nixpkgs/blob/0a51bb996cfaf87d903da5391a8b898c06886bcd/pkgs/development/haskell-modules/configuration-nix.nix#L112
        hie-bios = pkgs.haskell.lib.dontCheck (
          hself.callHackageDirect {
            pkg = "hie-bios";
            ver = "0.17.0";
            sha256 = "sha256-P4gEuewldhE/xd57xxH7Ho5IB931kPlptDtWNIT3j4Y=";
          } { }
        );
        # NOTE: https://github.com/NixOS/nixpkgs/blob/0a51bb996cfaf87d903da5391a8b898c06886bcd/pkgs/development/haskell-modules/configuration-nix.nix#L105
        hiedb =
          pkgs.haskell.lib.overrideCabal
            (hself.callHackageDirect {
              pkg = "hiedb";
              ver = "0.7.0.0";
              sha256 = "sha256-kdMmsvP3ofHZNzpCgdWO9kOZ1/hC7yRiFTy8K1X92kQ=";
            } { })
            (drv: {
              preCheck = ''
                export PATH=$PWD/dist/build/hiedb:$PATH
              '';
            });
        # NOTE: https://github.com/NixOS/nixpkgs/blob/0a51bb996cfaf87d903da5391a8b898c06886bcd/pkgs/development/haskell-modules/configuration-nix.nix#L100
        ghcide =
          pkgs.haskell.lib.overrideCabal
            (hself.callHackageDirect {
              pkg = "ghcide";
              ver = "2.12.0.0";
              sha256 = "sha256-uN//E/oQnotrTGVZ5Y1h1aCxLthrYvURERaV4Pe7Dhw=";
            } { })
            (drv: {
              preCheck = ''export PATH="$PWD/dist/build/ghcide:$PATH"'';
            });
        hls-graph = hself.callHackageDirect {
          pkg = "hls-graph";
          ver = "2.12.0.0";
          sha256 = "sha256-ELoMhRp57rcVEHEJRrcppP3Fc9ouwoMgbOuPUwSQ7sM=";
        } { };
        hls-plugin-api = hself.callHackageDirect {
          pkg = "hls-plugin-api";
          ver = "2.12.0.0";
          sha256 = "sha256-pZpOxJI2WSgF0x+zW7nlCvrdeOu0EdAFhwOVS6k+JGA=";
        } { };
        hls-test-utils = hself.callHackageDirect {
          pkg = "hls-test-utils";
          ver = "2.12.0.0";
          sha256 = "sha256-B9mlC364BTtUzjyzVeJVymmR0+cw9tB2/A+PezBl2nc=";
        } { };
        # NOTE: https://github.com/NixOS/nixpkgs/blob/09c221b2f0726da85b124efb60a1d123971dfa08/pkgs/development/haskell-modules/make-package-set.nix#L266

      };
      make-haskell-language-server =
        pkgs:
        let
          pkg = "haskell-language-server";
          ver = "2.12.0.0";
          pkgver = "${pkg}-${ver}";
        in
        (pkgs.haskell.packages."ghc${ghcVersion}".callCabal2nixWithOptions pkg
          (pkgs.fetchzip {
            url = "mirror://hackage/${pkgver}/${pkgver}.tar.gz";
            sha256 = "sha256-79uoFnat1Z8Q4gIyt0Poz5Lq2PruG0iRIFhItpu5PYc=";
          })
          # NOTE: https://haskell-language-server.readthedocs.io/en/latest/support/plugin-support.html
          # NOTE: retrie stan splice not supported for 9.12 yet
          "-f-floskell -f-stylishhaskell -f-fourmolu -f-cabalfmt -f-retrie -f-stan -f-splice"
          { }
        ).overrideScope
          (make-haskell-language-server-scope pkgs);
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
                  (make-haskell-language-server pkgs)
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
