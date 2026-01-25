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
  (builtins.removeAttrs args [
    "ghcVersion"
    "prerender"
    "wasm"
  ])
  // {
    overrides = lib.composeManyExtensions [
      (
        hself: hsuper:
        lib.attrsets.optionalAttrs prerender {
          miso = haskell.lib.enableCabalFlag hsuper.miso "ssr";
        }
      )
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
            ]
        )
      );
  }
)
