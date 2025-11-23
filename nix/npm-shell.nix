{
  mkShell,
  importNpmLock,
  nodejs,
  typescript,
  typescript-language-server,
  bun,
  emacs-lsp-booster,
  prettier,
}:
{ npmRoot }:
mkShell {
  packages = [
    nodejs
    importNpmLock.hooks.linkNodeModulesHook
    typescript
    typescript-language-server
    bun
    emacs-lsp-booster
    prettier
  ];
  npmDeps = importNpmLock.buildNodeModules { inherit npmRoot nodejs; };
}
