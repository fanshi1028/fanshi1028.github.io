{
  lib,
  writeShellApplication,
  cabal-install,
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
    flags = ''--watch app --test-ghci Main.main'';
  };
}
