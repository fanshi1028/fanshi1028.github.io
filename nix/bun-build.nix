{
  importNpmLock,
  runCommandLocal,
  lib,
  nodejs,
  bun,
  fetchurl,
}:
{
  name,
  src,
  minify ? true,
}:
let
  npmDeps = importNpmLock.buildNodeModules {
    npmRoot = src;
    inherit nodejs;
  };
  facility-hssp7-api-result = fetchurl {
    url = "https://www.lcsd.gov.hk/datagovhk/facility/facility-hssp7.json";
    hash = "sha256-3fj7qngz6tMBiAnKyUEB0UGiaYd2EHuqm4wszPmtYUU=";
  };
in
runCommandLocal "bun-build-${name}"
  {
    nativeBuildInputs = [ bun ];
    inherit src npmDeps;
  }
  ''
    cp -rv $src/* ./
    ${lib.optionalString (
      name == "maplibre-gl-ffi"
    ) "cp -v ${facility-hssp7-api-result} ./facility-hssp7.json"}

    mkdir $out

    NODE_PATH=$npmDeps/node_modules bun build index.ts \
           ${lib.optionalString minify "--minify"} \
           ${lib.optionalString (name == "wasm-entry") "--external ../all.js"} \
           --outdir $out

    ${lib.optionalString (
      name == "maplibre-gl-ffi"
    ) "cp -v $npmDeps/node_modules/maplibre-gl/dist/maplibre-gl.css $out/"}
  ''
