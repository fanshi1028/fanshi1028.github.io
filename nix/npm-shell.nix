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
  npmRoot ? null,
  noPackageJSON ? false, # NOTE: set this to true to just have a npm env to init your packge.
}:
assert !noPackageJSON -> npmRoot != null;
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
    ++ (lib.optional (noPackageJSON == false) importNpmLock.linkNodeModulesHook);
  }
  // (lib.optionalAttrs (noPackageJSON == false) {
    npmDeps = importNpmLock.buildNodeModules { inherit npmRoot nodejs; };
  })
)
