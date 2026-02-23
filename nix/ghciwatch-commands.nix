{
  lib,
  writeShellApplication,
  wasm32-wasi-cabal,
  bun,
  ghciwatch,
}:
writeShellApplication {
  runtimeInputs = [
    wasm32-wasi-cabal
    ghciwatch
    bun
    # tailwindcss
  ];
  text = ''
    ${ghciwatch.meta.mainProgram} \
     --command "${wasm32-wasi-cabal.meta.mainProgram} repl exe:fanshi1028-site -flocal-dev \
           --repl-options=-fghci-browser --repl-options=-fghci-browser-port=8080" \
     --clear \
     --watch src \
     --watch app \
     --test-ghci Main.main "$@"
  '';
  name = "ghciwatch-site";
  # ''--watch app --before-reload-shell "tailwindcss -i static/input.css -o static/output.css"'';

}
