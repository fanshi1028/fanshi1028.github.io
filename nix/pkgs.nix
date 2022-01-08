{ nixpkgsPin ? "2111" }:
let
  sources = import ./sources.nix { };
  haskellNix = import sources.haskellNix { };
in import haskellNix.sources."nixpkgs-${nixpkgsPin}" (haskellNix.nixpkgsArgs
  // {
    overlays = haskellNix.overlays ++ [
      (self: super: {
        haskell-nix = super.haskell-nix // {
          toolPackageName = super.haskell-nix.toolPackageName // {
            hakyll-init = "hakyll";
          };
          packageToolName = super.haskell-nix.packageToolName // {
            hakyll = "hakyll-init";
          };
        };
      })
    ];
  })
