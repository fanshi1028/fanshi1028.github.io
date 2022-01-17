# shell.nix
{ nixpkgsPin ? "2111", ghcVersion ? if nixpkgsPin == "2111" then
  "8107"
else if nixpkgsPin == "unstable" then
  "921"
else
  null, checkMaterialization ? false, checkProjectMaterialization ? false
, buildInputs ? [ ], modules ? [ ], pkg-def-extras ? [ ], additional ? (hs: [ ])
, index-state ? "2021-12-31T00:00:00Z"
, plan-sha256 ? "1q9d9q9i241j66xrcplbzc378msgrklv0vl5wxl60lzzxbi2vsng"
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
      plan-sha256 = "03i9rdvnpkr96x3ng5zfvfd9h49qsyzmxlckh2i1yr4xn991yid3";
      materialized = materializedDir + /cabal;
    };
    hlint = {
      inherit index-state checkMaterialization;
      version = "3.3.6";
      plan-sha256 = "17km3wxl79sl6vcgjl3yadqm41lb503hd8vsr9rc0z95yg20n91j";
      materialized = materializedDir + /hlint;
    };
    haskell-language-server = {
      inherit index-state checkMaterialization;
      version = "1.5.1.0";
      plan-sha256 = "0vhq2gvbwssgbh54ciwg2jcjsqsbrc9ml54yqnvv758ykga6xrsk";
      materialized = materializedDir + /haskell-language-server;
    };
    # error: builder for '/nix/store/9w46v4709ddiycqg6zdrssfwsjlz64nq-ormolu-lib-ormolu-0.4.0.0.drv' failed with exit code 1
    ormolu = {
      inherit index-state checkMaterialization;
      version = "0.4.0.0";
      plan-sha256 = "1g1g88bi46lx7kf2zc7lq7bgcqvcs5h7d53v5zclhgihfww1w5hl";
      materialized = materializedDir + /ormolu;
      # TEMP FIXME NOTE: https://github.com/input-output-hk/haskell.nix/issues/1337
      modules = [
        ({ lib, ... }: {
          options.nonReinstallablePkgs = lib.mkOption { apply = lib.remove "Cabal"; };
        })
      ];
    };
    ghcid = {
      inherit index-state checkMaterialization;
      version = "0.8.7";
      plan-sha256 = "0z35zpx7x0qncqiddnq4acdq3cgqlalwc2kb28dv8lk1rvn377ri";
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
      plan-sha256 = "1b5ckkajsf87jczavx18glwfa06zcvi7w1dp45xbpiyjqf7wmpi2";
      materialized = materializedDir + /stan;
    };
    hoogle = {
      inherit index-state checkMaterialization;
      version = "5.0.18.3";
      plan-sha256 = "0knhl9icjpmbqz18vw4pxs6n5m6m32b1jyss6cmlz86s6df7pwik";
      materialized = materializedDir + /hoogle;
    };
    hakyll = {
      inherit index-state checkMaterialization;
      version = "4.15.1.0";
      plan-sha256 = "1zs7b8wdlf3sv7lqhkjgpjbzynhxs8c7yxksibnjspl0796b4akk";
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
