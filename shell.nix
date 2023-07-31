# shell.nix
{ nixpkgsPin ? "unstable", ghcVersion ? "8107", checkMaterialization ? false
, checkProjectMaterialization ? false, buildInputs ? [ ], modules ? [ ]
, pkg-def-extras ? [ ], additional ? (hs: [ ])
, index-state ? "2022-02-01T00:00:00Z"
, plan-sha256 ? "1n13di297jzn2ywdjcmdzxzfwx4rz1axfyawz3ibpn3h48jl995h"
, materialized ? ./materialized/haskell-nix }:
assert ghcVersion != null;
let
  project = import ./default.nix {
    inherit nixpkgsPin ghcVersion modules index-state plan-sha256 materialized
      pkg-def-extras;
    checkMaterialization = checkProjectMaterialization;
  };
  materializedDir = ./materialized;
in project.shellFor {
  inherit additional;
  # ALL of these arguments are optional.

  # List of packages from the project you want to work on in
  # the shell (default is all the projects local packages).
  packages = ps: with ps; [ blog ];

  # Builds a Hoogle documentation index of all dependencies,
  # and provides a "hoogle" command to search the index.
  withHoogle = true;

  # Some common tools can be added with the `tools` argument
  tools = {
    cabal = {
      inherit index-state checkMaterialization;
      version = "3.6.2.0";
      plan-sha256 = "1ccslngsyc42bxbcx4hj20vx70ivvgp0rhd0kf8r5b85dxn3z7rb";
      materialized = materializedDir + /cabal;
    };
    hlint = {
      inherit index-state checkMaterialization;
      version = "3.3.6";
      plan-sha256 = "0qzcqh2g02kjdzkkhcm0qyjfsdvwk614fdpn8ql8syng1calhcy7";
      materialized = materializedDir + /hlint;
    };
    haskell-language-server = {
      inherit index-state checkMaterialization;
      version = "1.6.1.0";
      plan-sha256 = "03n82xn92flbi5nnnba6z73ar1qn4qnkfwk1amcxnn0x8bqy7xpg";
      materialized = materializedDir + /haskell-language-server;
    };
    # error: builder for '/nix/store/9w46v4709ddiycqg6zdrssfwsjlz64nq-ormolu-lib-ormolu-0.4.0.0.drv' failed with exit code 1
    ormolu = {
      inherit index-state checkMaterialization;
      version = "0.4.0.0";
      plan-sha256 = "0fp9hzdx9187i3nd4k0xkp0ynzsqxr2nff5g027mgr03n4vdqhld";
      materialized = materializedDir + /ormolu;
      # TEMP FIXME NOTE: https://github.com/input-output-hk/haskell.nix/issues/1337
      modules = [
        ({ lib, ... }: {
          options.nonReinstallablePkgs =
            lib.mkOption { apply = lib.remove "Cabal"; };
        })
      ];
    };
    ghcid = {
      inherit index-state checkMaterialization;
      version = "0.8.7";
      plan-sha256 = "1s21mdfqpc397wslqkmbazfvgcwwlnq4gfjd841v2q62nkbjdkps";
      materialized = materializedDir + /ghcid;
    };
    # use cabal-docspec instead of doctest(which I failed to set it up right), yet it seesm that cabal-docspec is not on hackage yet
    # https://github.com/phadej/cabal-extras/tree/master/cabal-docspec
    # doctest = "latest";
    cabal-fmt = {
      inherit index-state checkMaterialization;
      version = "0.1.5.1";
      plan-sha256 = "1wbds3wmnfzl9g562271bvd8z1whmiv2cn10m3qxbfpbdj3ri25h";
      materialized = materializedDir + /cabal-fmt;
    };
    stan = {
      inherit index-state checkMaterialization;
      version = "0.0.1.0";
      plan-sha256 = "06sim1kdr4qrricazds4ig7h5l8d9v0q0iivdbf2p8f3dqn3bh36";
      materialized = materializedDir + /stan;
    };
    hoogle = {
      inherit index-state checkMaterialization;
      version = "5.0.18.3";
      plan-sha256 = "16xvyynw5dmqlw1c02z2ym53izalfnciyjjac10k2ifdjqf9smma";
      materialized = materializedDir + /hoogle;
    };
    hakyll = {
      inherit index-state checkMaterialization;
      version = "4.15.1.0";
      plan-sha256 = "1wkfdidhdf3ng7qzcsf0z21403a6gqfbhw74kpdz71yvbyjqls79";
      materialized = materializedDir + /hakyll;
    };
  };
  # See overlays/tools.nix for more details

  # Some you may need to get some other way.
  buildInputs = with import ./nix/pkgs.nix { inherit nixpkgsPin; };
    [ ] ++ buildInputs;

  # Sellect cross compilers to include.
  crossPlatforms = ps:
    with ps;
    [
      # ghcjs # Adds support for `js-unknown-ghcjs-cabal build` in the shell
      # mingwW64 # Adds support for `x86_64-W64-mingw32-cabal build` in the shell
    ];

  # Prevents cabal from choosing alternate plans, so that
  # *all* dependencies are provided by Nix.
  exactDeps = true;
}
