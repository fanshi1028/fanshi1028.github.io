{
  lib,
  mkShell,
  importNpmLock,
  nodejs,
  typescript,
  typescript-language-server,
  bun,
  emacs-lsp-booster,
  prettier,
}:
{
  npmRoot,
  noPackageJSON ? false, # NOTE: set this to true to just have a npm env to init your packge.
}:
mkShell (
  {
    packages = [
      nodejs
      typescript
      typescript-language-server
      bun
      emacs-lsp-booster
      prettier
    ]
    ++ (lib.optional (noPackageJSON == false) importNpmLock.hooks.linkNodeModulesHook);
  }
  // (lib.optionalAttrs (noPackageJSON == false) {
    npmDeps = importNpmLock.buildNodeModules { inherit npmRoot nodejs; };
  })
)
