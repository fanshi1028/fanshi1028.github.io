{ nixpkgsPin ? "2111", ghcVersion ? if nixpkgsPin == "2111" then
  "8107"
else if nixpkgsPin == "unstable" then
  "921"
else
  null }:
assert ghcVersion != null;
with import ./nix/pkgs.nix { inherit nixpkgsPin; };
haskell-nix.project {
  # 'cleanGit' cleans a source directory based on the files known by git
  src = haskell-nix.haskellLib.cleanGit {
    name = "blog";
    src = ./.;
  };
  # Specify the GHC version to use.
  compiler-nix-name = "ghc${ghcVersion}";

  index-state = "2021-12-31T00:00:00Z";

  # plan-sha256 = "RiN1455qlA4rp2yO6wJp9FCVzAOR20KzfmNMM3fPXTQ=";

  modules = [ ];
}
