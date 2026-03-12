// NOTE:: https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/tree/master?ref_type=heads#what-it-emits-when-it-emits-a-wasm-file
const wasmFeaturesSupportedCheck = () =>
  import('wasm-feature-detect')
    .then((detect) =>
      Promise.all([
        // mandatory wasm extensions
        detect.saturatedFloatToInt() ||
          console.warn('WASM extension not supported: saturatedFloatToInt'),
        detect.signExtensions() ||
          console.warn('WASM extension not supported: signExtensions'),
        detect.mutableGlobals() ||
          console.warn('WASM extension not supported: mutableGlobals'),
        detect.referenceTypes() ||
          console.warn('WASM extension not supported: referenceTypes'),
        // optional wasm extensions enabled
        detect.multiValue() ||
          console.warn('WASM extension not supported: multiValue'),
        detect.bulkMemory() ||
          console.warn('WASM extension not supported: bulkMemory'),
        detect.simd() || console.warn('WASM extension not supported: simd'),
        // optional wasm extensions not enabled
        // detect.tailCall()|| console.info("WASM extension not supported: tailCall"),
      ])
    )
    .then((wasm_feature_detections) => wasm_feature_detections.every((i) => i))

if (await wasmFeaturesSupportedCheck()) {
  const browser_wasi_shim_imports = import('@bjorn3/browser_wasi_shim')
  const ghc_wasm_jsffi_imports = import('./ghc_wasm_jsffi.js')

  const args: string[] = []
  const env = ['GHCRTS=-H64m']

  const { WASI, OpenFile, File, ConsoleStdout } =
    await browser_wasi_shim_imports
  const fds = [
    new OpenFile(new File([])), // stdin
    ConsoleStdout.lineBuffered((msg) => console.log(`[WASI stdout] ''${msg}`)),
    ConsoleStdout.lineBuffered((msg) => console.warn(`[WASI stderr] ''${msg}`)),
  ]
  const options = { debug: false }
  const wasi = new WASI(args, env, fds, options)

  const instance_exports = {}
  const { instance } = await WebAssembly.instantiateStreaming(
    fetch('./fanshi1028-site.wasm'),
    {
      wasi_snapshot_preview1: wasi.wasiImport,
      ghc_wasm_jsffi: (await ghc_wasm_jsffi_imports)
        // @ts-ignore
        .default(instance_exports),
    }
  )
  Object.assign(instance_exports, instance.exports)

  // @ts-ignore
  wasi.initialize(instance)
  // @ts-ignore
  await instance.exports.hs_start()
} else {
  alert('The WASM site is not supported by you browser: Going back to the pure JS site')
  window.location.pathname = window.location.pathname.replace('wasm', '')
}
