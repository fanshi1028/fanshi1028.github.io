const browser_wasi_shim_imports = import("./browser_wasi_shim/dist/index.js");
const ghc_wasm_jsffi_imports = import("./ghc_wasm_jsffi.js");

const args = [];
const env = ["GHCRTS=-H64m"];

const { WASI, OpenFile, File, ConsoleStdout } = await browser_wasi_shim_imports
const fds = [
    new OpenFile(new File([])), // stdin
    ConsoleStdout.lineBuffered((msg) => console.log(`[WASI stdout] ''${msg}`)),
    ConsoleStdout.lineBuffered((msg) => console.warn(`[WASI stderr] ''${msg}`)),
];
const options = { debug: false };
const wasi = new WASI(args, env, fds, options);

const instance_exports = {};
const { instance } = await WebAssembly.instantiateStreaming(fetch("fanshi1028-site.wasm"), {
    wasi_snapshot_preview1: wasi.wasiImport,
    ghc_wasm_jsffi: (await ghc_wasm_jsffi_imports).default(instance_exports),
});
Object.assign(instance_exports, instance.exports);

wasi.initialize(instance);
await instance.exports.hs_start();

