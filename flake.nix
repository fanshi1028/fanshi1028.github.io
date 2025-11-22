{
  nixConfig.extra-substituters = [
    "https://fanshi1028-personal.cachix.org"
  ];

  inputs = {
    # copied from miso's flake
    nixpkgs.url = "github:nixos/nixpkgs?rev=9e2e8a7878573d312db421d69e071690ec34e98c";
    ghc-wasm.url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";
  };

  outputs =
    {
      self,
      nixpkgs,
      ghc-wasm,
    }:
    let
      ghcVersion = "9122";
      make-haskell-overrides =
        pkgs:
        import ./nix/haskell-overrides.nix {
          inherit (pkgs.lib) pipe;
          inherit (pkgs.haskell.lib) compose;
        };

      mkDefaultPackage =
        pkgs: args:
        pkgs.haskell.packages."ghc${ghcVersion}".developPackage (
          {
            root = ./.;
            source-overrides = {
              miso = pkgs.fetchFromGitHub {
                owner = "dmjio";
                repo = "miso";
                rev = "af221db695f7df4191f182a9458f708a4e6020ae";
                sha256 = "sha256-JsxFNgYITtPV4fCIsmhjz9aAMp0RP8ECuUdCUn3NkfU=";
              };
              cborg = "${
                pkgs.fetchFromGitHub {
                  owner = "well-typed";
                  repo = "cborg";
                  rev = "36eb23049ba4d0e33a8487420eb3b270899d64a7";
                  sha256 = "sha256-RKOmhcxj8HZ9NbvoYjclVKwvQcDxeRkEkvYdVyXJOH0=";
                }
              }/cborg";
              hashtables = "1.4.2";
            };
          }
          // args
          // {
            overrides =
              if args ? overrides then
                pkgs.lib.composeManyExtensions [
                  (make-haskell-overrides pkgs)
                  args.overrides
                ]
              else
                make-haskell-overrides pkgs;
          }
        );
    in
    {
      packages = builtins.mapAttrs (system: pkgs: {
        inherit (pkgs) tailwindcss closurecompiler;
        inherit
          (pkgs.haskell.packages."ghc${ghcVersion}".override {
            overrides = make-haskell-overrides pkgs;
          })
          miso # TEMP FIXME
          haxl
          cborg # TEMP FIXME
          ;

        prerender-js = mkDefaultPackage pkgs {
          modifier =
            drv:
            pkgs.lib.pipe drv [
              (pkgs.haskell.lib.compose.setBuildTargets [ "prerender" ])
              (pkgs.haskell.lib.compose.overrideCabal (_: {
                pname = "prerender";
              }))
            ];
        };

        prerender-wasm = mkDefaultPackage pkgs {
          modifier =
            drv:
            pkgs.lib.pipe drv [
              (pkgs.haskell.lib.compose.setBuildTargets [ "prerender" ])
              (pkgs.haskell.lib.compose.enableCabalFlag "prerender-wasm")
              (pkgs.haskell.lib.compose.overrideCabal (_: {
                pname = "prerender";
              }))
            ];
        };

        fanshi1028-site-js = mkDefaultPackage pkgs.pkgsCross.ghcjs {
          overrides = hself: hsuper: ({
            hashtables = pkgs.haskell.lib.enableCabalFlag hsuper.hashtables "portable";
            # NOTE: https://github.com/ghcjs/jsaddle/pull/162
            jsaddle = pkgs.haskell.lib.appendPatch hsuper.jsaddle (
              pkgs.fetchpatch {
                url = "https://patch-diff.githubusercontent.com/raw/ghcjs/jsaddle/pull/162.patch";
                hash = "sha256-jVaHy+7y4O6/jVx9CLIp/QHKRnL922ueLIGjP+Jd6b8=";
                stripLen = 1;
              }
            );
          });
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
      }) nixpkgs.legacyPackages;

      devShells = builtins.mapAttrs (system: pkgs: {
        without-build-tools = mkDefaultPackage pkgs { returnShellEnv = true; };
        default = mkDefaultPackage pkgs {
          overrides = hself: hsuper: {
            jsaddle = pkgs.lib.pipe hsuper.jsaddle (
              with pkgs.haskell.lib.compose;
              [
                (enableCabalFlag "call-stacks")
                (enableCabalFlag "check-unchecked")
              ]
            );
            hashtables = hsuper.hashtables_1_4_2;
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
        npm = {
          wasm-entry =
            let
              nodejs = pkgs.nodejs_24;
            in
            pkgs.mkShell {
              packages = [
                nodejs
              ]
              ++ (with pkgs; [
                importNpmLock.hooks.linkNodeModulesHook
                typescript
                typescript-language-server
                emacs-lsp-booster
                prettier
              ]);
              npmDeps = pkgs.importNpmLock.buildNodeModules {
                npmRoot = ./typescript/wasm-entry;
                inherit nodejs;
              };
            };
        };
      }) nixpkgs.legacyPackages;
    };
}
