{ nixpkgsPin ? "unstable", ghcVersion ? "8107", checkMaterialization ? true
, modules ? [ ], pkg-def-extras ? [ ], index-state ? "2022-02-01T00:00:00Z"
, plan-sha256 ? "1n13di297jzn2ywdjcmdzxzfwx4rz1axfyawz3ibpn3h48jl995h"
, materialized ? ./materialized/haskell-nix }:
assert ghcVersion != null;
with import ./nix/pkgs.nix { inherit nixpkgsPin; };
haskell-nix.project {
  inherit checkMaterialization index-state plan-sha256 materialized modules
    pkg-def-extras;

  # 'cleanGit' cleans a source directory based on the files known by git
  src = haskell-nix.haskellLib.cleanGit {
    name = "blog";
    src = ./.;
  };
  # Specify the GHC version to use.
  compiler-nix-name = "ghc${ghcVersion}";
}
