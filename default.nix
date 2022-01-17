{ nixpkgsPin ? "2111", ghcVersion ? if nixpkgsPin == "2111" then
  "8107"
else if nixpkgsPin == "unstable" then
  "921"
else
  null, checkMaterialization ? false, modules ? [ ], pkg-def-extras ? [ ]
, index-state ? "2021-12-31T00:00:00Z"
, plan-sha256 ? "1q9d9q9i241j66xrcplbzc378msgrklv0vl5wxl60lzzxbi2vsng"
, materialized ? ./materialized/haskell-nix }:
assert ghcVersion != null;
with import ./nix/pkgs.nix { inherit nixpkgsPin; };
haskell-nix.project {
  inherit checkMaterialization index-state plan-sha256 materialized modules pkg-def-extras;

  # 'cleanGit' cleans a source directory based on the files known by git
  src = haskell-nix.haskellLib.cleanGit {
    name = "blog";
    src = ./.;
  };
  # Specify the GHC version to use.
  compiler-nix-name = "ghc${ghcVersion}";
}
