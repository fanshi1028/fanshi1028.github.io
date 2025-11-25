{
  importNpmLock,
  runCommandLocal,
  lib,
  nodejs,
  bun,
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
in
runCommandLocal "bun-build-${name}"
  {
    nativeBuildInputs = [ bun ];
    inherit src npmDeps;
  }
  ''
    mkdir $out
    NODE_PATH=$npmDeps/node_modules bun build $src/index.ts --outdir $out ${lib.optionalString minify "--minify"}
  ''
