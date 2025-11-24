{
  lib,
  writeShellApplication,
  cabal-install,
  bun,
  ghciwatch,
}:
let
  make-ghciwatch-script =
    exe-name:
    {
      flags ? "",
      runtimeInputs ? [ ],
    }:
    lib.nameValuePair ("ghciwatch-" + exe-name) (writeShellApplication {
      runtimeInputs = [
        cabal-install
        ghciwatch
      ]
      ++ runtimeInputs;
      text = ''
        ghciwatch \
         --command "cabal repl exe:${exe-name} --flags=\"+local-dev\"" \
         --clear \
         --watch src \
         ${flags} \
         "$@" 
      '';
      name = "ghciwatch-" + exe-name;
    });
in
lib.mapAttrs' make-ghciwatch-script {
  prerender = {
    flags = ''--watch prerender'';
  };
  fanshi1028-site = {
    # flags = ''--watch app --before-reload-shell "tailwindcss -i static/input.css -o static/output.css"'';
    # runtimeInputs = [ tailwindcss ];
    flags = ''
      --watch app \
      --before-reload-shell "bun build typescript/maplibre-gl-ffi/index.ts --root typescript/maplibre-gl-ffi --outdir js-src --format iife --target browser" \
      --test-ghci Main.main
    '';
    runtimeInputs = [ bun ];
  };
}
