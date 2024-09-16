#include "Rts.h"
#include "Prelude.h"
#include "Schedule.h"
#include "sm/Sanity.h"

#if defined(__wasm_reference_types__)

extern HsBool rts_JSFFI_flag;
extern HsStablePtr rts_threadDelay_impl;
extern StgClosure ghczminternal_GHCziInternalziWasmziPrimziConcziInternal_threadDelay_closure;

int __main_void(void);

int __main_argc_argv(int, char*[]);

int __main_argc_argv(int argc, char *argv[]) {
  RtsConfig __conf = defaultRtsConfig;
  __conf.rts_opts_enabled = RtsOptsAll;
  __conf.rts_hs_main = false;
  hs_init_ghc(&argc, &argv, __conf);
  // See Note [threadDelay on wasm] for details.
  rts_JSFFI_flag = HS_BOOL_TRUE;
  rts_threadDelay_impl = getStablePtr((StgPtr)&ghczminternal_GHCziInternalziWasmziPrimziConcziInternal_threadDelay_closure);
  return 0;
}

// Note [JSFFI initialization]
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~
//
// In the wasi preview1 spec, there are two kinds of wasm32-wasi
// modules: commands and reactors. A command module exports _start()
// that's intended to be invoked exactly once, after which all
// instance state is invalidated. A reactor module supports
// user-defined entrypoints that can be called multiple times after
// _initialize() has been called. In Haskell, when JSFFI is used, we
// expect user-defined JSFFI exports to be entrypoints that can be
// called from JavaScript multiple times, in which case the user needs
// to target wasm32-wasi reactor modules by passing the correct
// link-time options, see user manual for details.
//
// What about RTS initialization? We can tell users to export
// functions like hs_init() as well and call that before calling JSFFI
// exports, but this is very inconvenient. So in this case, we choose
// to do it via a ctor defined here. When JSFFI is not used, JSFFI.o
// will not be included by wasm-ld so this ctor will not get in the
// way. When JSFFI is indeed used, this ctor will be called by the
// linker generated __wasm_call_ctors() function, which is called by
// _initialize(), so the user only needs to call _initialize() once
// and then they can call user exported functions directly.
//
// When it comes to ctors, we must pay close attention to ctor
// priorities to guarantee they are invoked in the correct order. Both
// wasi-libc and emscripten libc makes use of ctors to initialize some
// libc state, and we want them to be invoked first, so priority range
// [0..100] has been fully booked. If priority is ommited, it defaults
// the lowest value 65535, and now there's a problem: there are other
// ctors generated by the GHC codegen (e.g. registering foreign export
// closures as GC roots), and we must ensure those ctors are invoked
// before our RTS initialization logic kicks in!
//
// Therefore, on wasm32, we designate priority 101 to ctors generated
// by the GHC codegen, and priority 102 to the initialization logic
// here to ensure hs_init_ghc() sees everything it needs to see.
__attribute__((constructor(102))) static void __ghc_wasm_jsffi_init(void) {
  // See
  // https://gitlab.haskell.org/ghc/wasi-libc/-/blob/main/libc-bottom-half/sources/__main_void.c
  // for its definition. It initializes some libc state, then calls
  // __main_argc_argv defined above.
  __main_void();
}

typedef __externref_t HsJSVal;
typedef StgWord JSValKey;

extern const StgInfoTable stg_JSVAL_info;
extern const StgInfoTable ghczminternal_GHCziInternalziWasmziPrimziTypes_JSVal_con_info;

// See Note [JSVal representation for wasm] for detailed explanation.

__attribute__((import_module("ghc_wasm_jsffi"), import_name("newJSVal")))
JSValKey __imported_newJSVal(HsJSVal);

__attribute__((import_module("ghc_wasm_jsffi"),
               import_name("freeJSVal"))) void __imported_freeJSVal(JSValKey);

static void __wrapped_freeJSVal(JSValKey k) {
  __imported_freeJSVal(k);
}

HaskellObj rts_mkJSVal(Capability*, HsJSVal);
HaskellObj rts_mkJSVal(Capability *cap, HsJSVal v) {
  JSValKey k = __imported_newJSVal(v);

  HaskellObj p = (HaskellObj)allocate(cap, CONSTR_sizeW(0, 1));
  SET_HDR(p, &stg_JSVAL_info, CCS_SYSTEM);
  p->payload[0] = (HaskellObj)k;

  StgCFinalizerList *cfin =
      (StgCFinalizerList *)allocate(cap, sizeofW(StgCFinalizerList));
  SET_HDR(cfin, &stg_C_FINALIZER_LIST_info, CCS_SYSTEM);
  cfin->link = &stg_NO_FINALIZER_closure;
  cfin->fptr = (void (*)(void))__wrapped_freeJSVal;
  cfin->ptr = (void *)k;
  cfin->flag = 0;

  StgWeak *w = (StgWeak *)allocate(cap, sizeofW(StgWeak));
  SET_HDR(w, &stg_WEAK_info, CCS_SYSTEM);
  w->cfinalizers = (StgClosure *)cfin;
  w->key = p;
  w->value = Unit_closure;
  w->finalizer = &stg_NO_FINALIZER_closure;
  w->link = cap->weak_ptr_list_hd;
  cap->weak_ptr_list_hd = w;
  if (cap->weak_ptr_list_tl == NULL) {
    cap->weak_ptr_list_tl = w;
  }

  HaskellObj box = (HaskellObj)allocate(cap, CONSTR_sizeW(3, 0));
  SET_HDR(box, &ghczminternal_GHCziInternalziWasmziPrimziTypes_JSVal_con_info, CCS_SYSTEM);
  box->payload[0] = p;
  box->payload[1] = (HaskellObj)w;
  box->payload[2] = NULL;
  return TAG_CLOSURE(1, box);
}

__attribute__((import_module("ghc_wasm_jsffi"), import_name("getJSVal")))
HsJSVal __imported_getJSVal(JSValKey);

STATIC_INLINE HsJSVal rts_getJSValzh(HaskellObj p) {
  ASSERT(p->header.info == &stg_JSVAL_info);
  return __imported_getJSVal((JSValKey)p->payload[0]);
}

HsJSVal rts_getJSVal(HaskellObj);
HsJSVal rts_getJSVal(HaskellObj box) {
  ASSERT(UNTAG_CLOSURE(box)->header.info == &ghczminternal_GHCziInternalziWasmziPrimziTypes_JSVal_con_info);
  return rts_getJSValzh(UNTAG_CLOSURE(box)->payload[0]);
}

INLINE_HEADER void pushClosure   (StgTSO *tso, StgWord c) {
  tso->stackobj->sp--;
  tso->stackobj->sp[0] = (W_) c;
}

extern const StgInfoTable stg_jsffi_block_info;
extern const StgInfoTable stg_scheduler_loop_info;
extern StgClosure ghczminternal_GHCziInternalziWasmziPrimziImports_raiseJSException_closure;

// schedule a future round of RTS scheduler loop via setImmediate(),
// to avoid jamming the JavaScript main thread

__attribute__((import_module("ghc_wasm_jsffi"), import_name("scheduleWork")))
void __imported_scheduleWork(void);

void rts_scheduleWork(void);
void rts_scheduleWork(void) {
  __imported_scheduleWork();
}

__attribute__((export_name("rts_schedulerLoop")))
void rts_schedulerLoop(void);
void rts_schedulerLoop(void) {
  Capability *cap = rts_lock();
  StgTSO *tso = createThread(cap, RESERVED_STACK_WORDS);
  pushClosure(tso, (StgWord)&stg_scheduler_loop_info);
  scheduleWaitThread(tso, NULL, &cap);
  rts_checkSchedStatus("rts_schedulerLoop", cap);
  rts_unlock(cap);
}

#define mk_rtsPromiseCallback(obj)                         \
  {                                                        \
  Capability *cap = &MainCapability;                       \
  StgTSO *tso = (StgTSO*)deRefStablePtr(sp);               \
  IF_DEBUG(sanity, checkTSO(tso));                         \
  hs_free_stable_ptr(sp);                                  \
                                                           \
  StgStack *stack = tso->stackobj;                         \
  IF_DEBUG(sanity, checkSTACK(stack));                     \
                                                           \
  if (stack->sp[0] == (StgWord)&stg_jsffi_block_info) {    \
    dirty_TSO(cap, tso);                                   \
    dirty_STACK(cap, stack);                               \
    stack->sp[1] = (StgWord)(obj);                         \
  }                                                        \
  scheduleThreadNow(cap, tso);                             \
  rts_schedulerLoop();                                     \
  }

#define mk_rtsPromiseResolve(T)                            \
  __attribute__((export_name("rts_promiseResolve"#T)))     \
  void rts_promiseResolve##T(HsStablePtr, Hs##T);          \
  void rts_promiseResolve##T(HsStablePtr sp, Hs##T js_res) \
  mk_rtsPromiseCallback(rts_mk##T(cap, js_res))

__attribute__((export_name("rts_promiseResolveUnit")))
void rts_promiseResolveUnit(HsStablePtr);
void rts_promiseResolveUnit(HsStablePtr sp)
  mk_rtsPromiseCallback(TAG_CLOSURE(1, Unit_closure))

mk_rtsPromiseResolve(JSVal)
mk_rtsPromiseResolve(Char)
mk_rtsPromiseResolve(Int)
mk_rtsPromiseResolve(Int8)
mk_rtsPromiseResolve(Int16)
mk_rtsPromiseResolve(Int32)
mk_rtsPromiseResolve(Int64)
mk_rtsPromiseResolve(Word)
mk_rtsPromiseResolve(Word8)
mk_rtsPromiseResolve(Word16)
mk_rtsPromiseResolve(Word32)
mk_rtsPromiseResolve(Word64)
mk_rtsPromiseResolve(Ptr)
mk_rtsPromiseResolve(FunPtr)
mk_rtsPromiseResolve(Float)
mk_rtsPromiseResolve(Double)
mk_rtsPromiseResolve(StablePtr)
mk_rtsPromiseResolve(Bool)

__attribute__((export_name("rts_promiseReject")))
void rts_promiseReject(HsStablePtr, HsJSVal);
void rts_promiseReject(HsStablePtr sp, HsJSVal js_err)
  mk_rtsPromiseCallback(rts_apply(cap, &ghczminternal_GHCziInternalziWasmziPrimziImports_raiseJSException_closure, rts_mkJSVal(cap, js_err)))

__attribute__((export_name("rts_freeStablePtr")))
void rts_freeStablePtr(HsStablePtr);
void rts_freeStablePtr(HsStablePtr sp) {
  hs_free_stable_ptr(sp);
}

#endif // __wasm_reference_types__
