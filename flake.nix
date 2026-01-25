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
    in
    {
      packages = builtins.mapAttrs (system: pkgs: {
        inherit (pkgs) tailwindcss closurecompiler;
        inherit
          (
            let
              hsPkgs = pkgs.haskell.packages."ghc${ghcVersion}";
            in
            hsPkgs.override {
              overrides = pkgs.lib.composeExtensions (hsPkgs.packageSourceOverrides (
                pkgs.callPackage ./nix/haskell-source-overrides.nix { }
              )) (pkgs.callPackage ./nix/haskell-overrides.nix { });
            }
          )
          miso
          haxl
          cborg
          ;

        prerender-js = pkgs.callPackage ./nix/fanshi1028-site.nix { } {
          inherit ghcVersion;
          root = ./.;
          prerender = true;
        };

        prerender-wasm = pkgs.callPackage ./nix/fanshi1028-site.nix { } {
          inherit ghcVersion;
          root = ./.;
          prerender = true;
          wasm = true;
        };

        fanshi1028-site-js = (
          pkgs.pkgsCross.ghcjs.callPackage ./nix/fanshi1028-site.nix { } {
            inherit ghcVersion;
            root = ./.;
          }
        );

        bun-build = pkgs.lib.attrsets.mapAttrs (
          path: type:
          if type != "directory" then
            null
          else
            pkgs.callPackage ./nix/bun-build.nix { nodejs = pkgs.nodejs_24; } {
              name = path;
              src = ./typescript/${path};
            }
        ) (builtins.readDir ./typescript);

      }) nixpkgs.legacyPackages;

      devShells = builtins.mapAttrs (system: pkgs: {
        default = pkgs.callPackage ./nix/fanshi1028-site.nix { } {
          inherit ghcVersion;
          root = ./.;
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
              ++ (lib.attrVals [ "ghciwatch-fanshi1028-site" "ghciwatch-prerender" ] (
                callPackage ./nix/ghciwatch-commands.nix { }
              ))
            );
          returnShellEnv = true;
        };
        wasm =
          let
            ghc-wasm = ghc-wasm.packages.${system};
          in
          pkgs.mkShell {
            name = "The miso ${system} GHC WASM ${ghcVersion} shell";
            packages = [
              ghc-wasm.all_9_12
            ]
            ++ (pkgs.lib.attrVals [ "ghciwatch-fanshi1028-site" ] (
              pkgs.callPackage ./nix/ghciwatch-commands.nix { cabal-install = ghc-wasm.wasm32-wasi-cabal-9_12; }
            ));
          };
        npm = pkgs.lib.attrsets.mapAttrs (
          path: type:
          if type != "directory" then
            null
          else
            pkgs.callPackage ./nix/npm-shell.nix { nodejs = pkgs.nodejs_24; } {
              npmRoot = ./typescript/${path};
            }
        ) (builtins.readDir ./typescript);
      }) nixpkgs.legacyPackages;
    };
}
