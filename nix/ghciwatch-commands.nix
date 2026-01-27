{
  lib,
  writeShellApplication,
  cabal-install,
  bun,
  ghciwatch,
}:
let
  cabal = cabal-install.meta.mainProgram;
  wasm = cabal == "wasm32-wasi-cabal";
in
writeShellApplication {
  runtimeInputs = [
    cabal-install
    ghciwatch
    bun
    # tailwindcss
  ];
  text = ''
    ghciwatch \
     --command "${cabal} repl exe:fanshi1028-site \
     --flags=\"+local-dev\" \
     ${lib.optionalString wasm "--repl-options=\\\"-fghci-browser -fghci-browser-port=8080\\\""}" \
     --clear \
     --watch src \
     --watch app \
     --before-reload-shell "bun build typescript/maplibre-gl-ffi/index.ts --root typescript/maplibre-gl-ffi --outdir ./typescript/maplibre-gl-ffi" \
     --test-ghci Main.main "$@"
  '';
  name = "ghciwatch-site";
  # ''--watch app --before-reload-shell "tailwindcss -i static/input.css -o static/output.css"'';

}
