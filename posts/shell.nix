# shell.nix
{ nixpkgsPin ? "2111", ghcVersion ? if nixpkgsPin == "2111" then
  "8107"
else if nixpkgsPin == "unstable" then
  "921"
else
  null }:
assert ghcVersion != null;
let
  project = import ../default.nix { inherit nixpkgsPin ghcVersion; };
  pkgs = import ../nix/pkgs.nix { inherit nixpkgsPin; };
  ghc = project.ghcWithPackages (ps:
    with ps; [
      random
      (pkgs.haskell-nix.hackage-package {
        name = "criterion";
        compiler-nix-name = "ghc${ghcVersion}";
      })
    ]);
  pkgsPackagesDevEnv = with pkgs; [
    # js
    nodejs
    # python
    python3
    pyright
    # clojure
    # leiningen
    clojure
    clj-kondo
  ];
  nodePackagesDevEnv = with pkgs.nodePackages; [
    prettier
    typescript
    typescript-language-server
  ];
  pythonPackagesDevEnv = with pkgs.python39Packages; [
    poetry
    pyflakes
    isort
    black
  ];
  nonHaskellDevEnv = pkgsPackagesDevEnv ++ nodePackagesDevEnv
    ++ pythonPackagesDevEnv;
  # in pkgs.mkShell { buildInputs = nonHaskellDevEnv ++ [ ghc ]; }
in import ../shell.nix {
  inherit nixpkgsPin ghcVersion;
  buildInputs = nonHaskellDevEnv;
  additional = ps:
    with ps; [
      random
      (pkgs.haskell-nix.hackage-package {
        name = "criterion";
        compiler-nix-name = "ghc${ghcVersion}";
      })
    ];
}
