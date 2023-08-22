{
  outputs = { self, nixpkgs-unstable, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      with import nixpkgs-unstable { inherit system; }; {
        devShells.default = mkShell {
          buildInputs = # [ typescript ] ++
            (with nodePackages; [
              pnpm
              svelte-check
              svelte-language-server
              prettier
              nodejs
            ]);
        };
      });
}
