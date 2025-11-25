{
  callPackage,
  lib,
  haskell,
  fetchpatch,
  stdenv,
}:
{
  root,
  ghcVersion,
  modifier ? drv: drv,
  overrides ? hself: hsuper: { },
  prerender ? false,
  wasm ? false,
  ...
}@args:
assert wasm -> prerender; # NOTE nix build for wasm site is not supported yet
haskell.packages."ghc${ghcVersion}".developPackage (
  {
    source-overrides = callPackage ./haskell-source-overrides.nix { };
  }
  // (builtins.removeAttrs args [
    "ghcVersion"
    "prerender"
    "wasm"
  ])
  // {
    overrides = lib.composeManyExtensions [
      (
        if stdenv.hostPlatform == "ghcjs" && !prerender then
          hself: hsuper: ({
            hashtables = haskell.lib.enableCabalFlag hsuper.hashtables "portable";
            # NOTE: https://github.com/ghcjs/jsaddle/pull/162
            jsaddle = haskell.lib.appendPatch hsuper.jsaddle (fetchpatch {
              url = "https://patch-diff.githubusercontent.com/raw/ghcjs/jsaddle/pull/162.patch";
              hash = "sha256-jVaHy+7y4O6/jVx9CLIp/QHKRnL922ueLIGjP+Jd6b8=";
              stripLen = 1;
            });
          })
        else
          hself: hsuper: { }
      )
      (callPackage ./haskell-overrides.nix { })
      overrides
    ];

    modifier =
      drv:
      modifier (
        lib.pipe drv (
          with haskell.lib.compose;
          if prerender then
            if wasm then
              [
                (setBuildTargets [ "prerender" ])
                (enableCabalFlag "prerender-wasm")
                (overrideCabal (_: {
                  pname = "prerender";
                }))
              ]
            else
              [
                (setBuildTargets [ "prerender" ])
                (overrideCabal (_: {
                  pname = "prerender";
                }))
              ]
          else if wasm then
            [ ] # NOTE: impossible case
          else
            [
              (enableCabalFlag "production")
              (setBuildTargets [ "exe:fanshi1028-site" ])
              (appendConfigureFlag "--ghc-options=-v2")
            ]
        )
      );
  }
)
