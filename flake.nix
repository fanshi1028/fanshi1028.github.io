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
      ghcVersion = "9122";
      overlays = [ (import "${miso}/nix/overlay.nix") ];
      applyFixToHaxl =
        pkgs: haxl:
        nixpkgs.lib.pipe haxl (
          with pkgs.haskell.lib.compose;
          [
            unmarkBroken
            (overrideCabal (drv: {
              # NOTE: https://github.com/facebook/Haxl/issues/165
              postPatch = ''
                substituteInPlace Setup.hs --replace-fail Setup Main
                sed -i 's/time >= 1.4 \&\& < 1.13/time >= 1.4 \&\& < 1.15/g' haxl.cabal
              '';
            }))
          ]
        );
      mkDefaultPackage =
        pkgs: args:
        pkgs.haskell.packages."ghc${ghcVersion}".developPackage (
          {
            root = ./.;
          }
          // args
          // {
            overrides =
              hself: hsuper:
              (
                {
                  haxl = applyFixToHaxl pkgs hsuper.haxl;
                }
                // (if args ? overrides then args.overrides hself hsuper else { })
              );
          }
        );
    in
    {
      packages = builtins.mapAttrs (
        system: pkgs:
        let
          pkgsWithMisoOverlays = import nixpkgs { inherit system overlays; };
        in
        {
          inherit (pkgs) tailwindcss closurecompiler;

          miso = pkgsWithMisoOverlays.haskell.packages."ghc${ghcVersion}".miso;
          haxl = applyFixToHaxl pkgs pkgsWithMisoOverlays.haskell.packages."ghc${ghcVersion}".haxl;

          prerender = mkDefaultPackage pkgsWithMisoOverlays {
            modifier =
              drv:
              pkgs.lib.pipe drv [
                (pkgs.haskell.lib.compose.setBuildTargets [ "prerender" ])
                (pkgs.haskell.lib.compose.overrideCabal (_: {
                  pname = "prerender";
                }))
              ];
          };
          fanshi1028-site-js = mkDefaultPackage pkgsWithMisoOverlays.pkgsCross.ghcjs {
            overrides = hself: hsuper: {
              zlib =
                (pkgs.lib.pipe hsuper.zlib (with pkgs.haskell.lib.compose; [ (enableCabalFlag "bundled-c-zlib") ]))
                .overrideAttrs
                  (oldAttrs: {
                    buildInputs = pkgs.lib.list.remove pkgs.zlib oldAttrs.buildInputs;
                  });
            };
            modifier =
              drv:
              pkgs.lib.pipe drv (
                with pkgs.haskell.lib.compose;
                [
                  (enableCabalFlag "production")
                  (setBuildTargets [ "exe:fanshi1028-site" ])
                ]
              );
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
      ) nixpkgs.legacyPackages;

      devShells = builtins.mapAttrs (
        system: pkgs:
        let
          pkgsWithMisoOverlays = import nixpkgs { inherit system overlays; };
        in
        {
          without-build-tools = mkDefaultPackage pkgsWithMisoOverlays { returnShellEnv = true; };
          default = mkDefaultPackage pkgsWithMisoOverlays {
            overrides = hself: hsuper: {
              jsaddle = pkgs.lib.pipe hsuper.jsaddle (
                with pkgs.haskell.lib.compose;
                [
                  (enableCabalFlag "call-stacks")
                  (enableCabalFlag "check-unchecked")
                ]
              );
            };
            modifier =
              drv:
              pkgs.haskell.lib.addBuildTools drv (
                with pkgs;
                [
                  cabal-install
                  tailwindcss
                  ghciwatch
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
                  (haskell-language-server.override {
                    supportedGhcVersions = [ ghcVersion ];
                    supportedFormatters = [ "ormolu" ];
                  })
                  emacs-lsp-booster
                ]
                ++ (with haskell.packages."ghc${ghcVersion}"; [
                  cabal-gild_1_6_0_0
                  ormolu_0_8_0_0
                ])
              );
            returnShellEnv = true;
          };
          wasm = pkgs.mkShell {
            name = "The miso ${system} GHC WASM ${ghcVersion} shell";
            packages = [ ghc-wasm.packages.${system}.all_9_12 ];
          };
        }
      ) nixpkgs.legacyPackages;
    };
}
