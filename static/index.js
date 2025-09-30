// NOTE:: https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/tree/master?ref_type=heads#what-it-emits-when-it-emits-a-wasm-file
import {
    // mandatory wasm extensions 
    saturatedFloatToInt,
    signExtensions,
    mutableGlobals,
    referenceTypes,
    // optional wasm extensions enabled
    multiValue,
    bulkMemory,
    simd,
    // optional wasm extensions not enabled
    // tailCall
} from "./wasm-feature-detect/dist/esm/index.js";

const wasm_feature_detections = await Promise.all([
    saturatedFloatToInt(),
    signExtensions(),
    mutableGlobals(),
    referenceTypes(),
    multiValue(),
    bulkMemory(),
    simd(),
])

if (wasm_feature_detections.every(i => i)) {
    await import("./wasm.js")
} else {
    await import("./all.js")
}


