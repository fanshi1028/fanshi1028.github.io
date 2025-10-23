//#OPTIONS:EMCC:EXPORTED_RUNTIME_METHODS=HEAP8
// NOTE: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/javascript.html#emcc-pragmas
// NOTE: https://gitlab.haskell.org/ghc/ghc/-/commit/2a411fc45d19a37615a6a47e0530c014c244bdf4

// Used by haxl's profiling code, even if the profileflags is disabled and hence cause runtime error:
// uncaught exception in Haskell thread: ReferenceError: h$stg_getThreadAllocationCounterzh is not defined
function h$stg_getThreadAllocationCounterzh() {
  // XXX dummy implementation
  return 0
}
