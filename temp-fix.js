//#OPTIONS:EMCC:EXPORTED_RUNTIME_METHODS=HEAP8
// NOTE: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/javascript.html#emcc-pragmas
// NOTE: https://gitlab.haskell.org/ghc/ghc/-/commit/2a411fc45d19a37615a6a47e0530c014c244bdf4

//////////////////
// Used by haxl //
//////////////////

// uncaught exception in Haskell thread: ReferenceError: h$stg_getThreadAllocationCounterzh is not defined
const h$stg_getThreadAllocationCounterzh = _stg_getThreadAllocationCounterzh

// warning, unhandled primop: setThreadAllocationCounter# (0,2)
// uncaught exception in Haskell thread: ReferenceError: h$primop_setThreadAllocationCounterzh is not defined
// ReferenceError: h$primop_setThreadAllocationCounterzh is not defined
const h$primop_setThreadAllocationCounterzh =
  _primop_setThreadAllocationCounterzh
