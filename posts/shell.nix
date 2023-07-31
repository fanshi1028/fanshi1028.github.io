# shell.nix
{ nixpkgsPin ? "unstable", ghcVersion ? "8107"}:
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
    # clj2nix
    # scala
    ammonite
    # rust
    cargo
    # https://github.com/oxalica/rust-overlay/issues/20#issuecomment-1013433604
    (rust-bin.stable.latest.default.override {
      extensions = [ "rust-src" "rustfmt" ];
    })
    rust-analyzer
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
