/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2013
 *
 * Performing updates.
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#if !defined(CMINUSMINUS)
#include "BeginPrivate.h"
#endif

/*
 *
 * Note [Thunks, blackholes, and indirections]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 *
 * Consider the following STG binding:
 *
 *     thnk = {fv_0, fv_1} \u [] g x y;
 *
 * This binding is a updatable thunk carrying free variables `fv_0` and `fv_1`.
 * Over its lifetime, this closure may transition through three states:
 *
 *  1. it starts life as a thunk, which carries with it free variables
 *  2. if a thread enters it, it may turn into a blackhole
 *  3. if evaluation finishes, it will be "updated", turning it into an indirection
 *     pointing to the result of the evaluation
 *
 * On the heap, state (1) is represented as a closure with the following layout
 * (embodied in the C runtime by the `StgThunk` struct):
 *
 *      thnk
 *     ┌───────────────────────┐  ╮
 *     │ blah_info             │  │
 *     ├────────────┬──────────┤  │ StgThunkHeader
 *     │ indirectee │ NULL     │  │
 *     ├────────────┼──────────┤  ╯
 *     │ payload[0] │ fv_0     │
 *     ├────────────┼──────────┤
 *     │ payload[1] │ fv_1     │
 *     └────────────┴──────────┘
 *
 * Here `blah_info` is a pointer to the thunk's info table, which will be of
 * type THUNK. The `indirectee` field (also known as the "SMP header") is
 * initially NULL and is unused while the closure remains a thunk. However, as
 * we will see, it will eventually point to the result of the thunk's
 * evaluation.
 *
 *
 * Entry
 * -----
 * As usual, to enter `thnk` (step (2) of the lifetime) a mutator thread
 * `tso_0` will jump to its entry code (which is recorded in or next to its
 * info table, depending upon whether tables-next-to-code is enabled). This
 * entry code will push an "update frame" (namely, either `stg_upd_frame` or
 * `stg_bh_upd_frame`) to `tso_0`'s stack and begin the evaluation of the
 * thunk's RHS.
 *
 * However, before commencing evaluation, the entry code may also mark the
 * thunk as being under evaluation; this process is known a "blackholing".
 * Whether this happens depends upon which of GHC's two blackholing strategies
 * which was selected during the compilation of the defining module. We will
 * discuss the simpler "eager blackholing" case first and later introduce the
 * more-efficient "lazy blackholing" strategy.
 *
 *
 * Eager blackholing
 * -----------------
 * Under the eager blackholing strategy (which is enabled via the
 * `-feager-blackholing` flag), a thunk's entry code (generated by
 * `GHC.StgToCmm.Bind.emitBlackHoleCode`) will immediately turn the thunk into
 * a blackhole, indicating that the thunk is under evaluation. Additionally,
 * the indirectee field will be updated to point to the thread performing the
 * evaluation, `tso_0`. Since we know statically that the thunk is now a
 * `BLACKHOLE`, the thunk entry code will push an `stg_bh_upd_frame` to the
 * stack in this case (in contrast to the lazy strategy, as we will see later).
 *
 * After this `thnk` will look like,
 *
 *      thnk
 *     ┌───────────────────────┐
 *     │ EAGER_BLACKHOLE_info  │
 *     ├────────────┬──────────┤           tso_0
 *     │ indirectee │ tso_0    │─────────►┌──────┐
 *     ├────────────┼──────────┤          │ ...  │
 *     │ payload[0] │ fv_0     │          └──────┘
 *     ├────────────┼──────────┤
 *     │ payload[1] │ fv_1     │
 *     └────────────┴──────────┘
 *
 * Note that blackholing in this way does not guarantee mutual exclusion: Two
 * threads may indeed race to enter `thnk`. This will result in both threads
 * performing evaluation and, in some cases, the blackhole being updated
 * multiple times.
 *
 *
 * Updating a thunk
 * ----------------
 * When `tso_0` finishes the evaluation of `thnk`, it will return to the entry
 * code of the update frame pushed when the thunk was entered (e.g.
 * `stg_bh_upd_frame`). This code first checks whether the blackhole
 * has already been updated by another thread; if it has then `tso_0` will
 * throw out its result and reuse that which the earlier thread recorded in the
 * blackhole's `indirectee` field.
 *
 * If the blackhole has not yet been updated then `tso_0` will:
 *
 *  1. set `thnk`'s `indirectee` field to point to the result of its
 *     evaluation, and
 *  2. set its info table to `BLACKHOLE_info`
 *
 * N.B. You might notice that step (2) here appears to be redundant as we
 * already set the info table pointer to `EAGER_BLACKHOLE_info` above. However,
 * as we will see below, this may not be the case when lazy blackholing is in
 * use.
 *
 * After these updates we will have the following:
 *
 *      thnk
 *     ┌───────────────────────┐
 *     │ BLACKHOLE_info        │
 *     ├────────────┬──────────┤           result
 *     │ indirectee │ result   │─────────►┌────────┐
 *     ├────────────┼──────────┤          │ ...    │
 *     │ payload[0] │ fv_0     │          └────────┘
 *     ├────────────┼──────────┤
 *     │ payload[1] │ fv_1     │
 *     └────────────┴──────────┘
 *
 * In addition, the code will check the blocking queues that were added to the
 * blackhole (recorded in the `indirectee` field, as we will see below) and
 * wake them up (see `Threads.c:updateThunk`).
 *
 * Note that we are using `BLACKHOLE_info` to represent two distinct states of
 * a thunk:
 *
 *  - if the indirectee points to a `TSO` or `BLOCKING_QUEUE`, then the
 *    `BLACKHOLE` represents a thunk currently being evaluated
 *
 *  - otherwise `BLACKHOLE` is merely representing an evaluated thunk and
 *    serves as an indirection to the final result.
 *
 * This overloading may seem odd given that we also have `stg_IND_info`, which
 * also represents an indirection.  However, the overloading serves a purpose:
 * it means that safely updating a blackhole (step (3) of the
 * lifetime above) requires only a single store (namely the store to the
 * `indirectee` field).
 *
 * If we were to instead use `stg_IND` to represent the updated thunk, we would
 * require two stores and consequently have an awkward period where the info
 * table and indirectee fields are inconsistent:
 *
 *  - if we were to update the info table first, there would be a period where
 *    the `indirectee` field pointed to the TSO which did the evaluation and
 *    not the result as one would expect.
 *
 *  - if we were to update the indirectee first, there would be a period where
 *    the closure is still a `BLACKHOLE_info` yet the indirectee points to the
 *    evaluation result.
 *
 * For this reason, it is simpler to use `BLACKHOLE` to represent both states
 * (2) and (3), distinguishing them using the identity of the indirectee. The
 * uses of `stg_IND` are few and will be discussed below.
 *
 *
 * Lazy blackholing
 * ----------------
 * While the strict blackholing strategy described above is simple and is
 * faithful to the semantics of the STG machine, it is fairly costly on modern
 * hardware. Specifically, thunk entry can be extremely common and in a
 * parallel program blackholing may induce considerable pressure on the
 * machine's memory subsystem.
 *
 * To mitigate this GHC by default uses a lazy blackholing strategy. Here we
 * take advantage of the fact that redundant evaluation of a thunk is
 * acceptable and defer blackholing until the thread returns to the scheduler.
 * This is an optimisation as frequently we will finish evaluation *before*
 * yielding; in this case we avoid incurring the memory writes necessary to
 * blackhole the thunk (step (2)) and rather update the thunk straight to an
 * indirection.
 *
 * When entering a thunk compiled with lazy blackholing, we push an
 * `stg_upd_frame` (not `stg_upd_bh_frame`) frame to the stack and do not
 * modify the thunk closure itself.
 *
 * If the thread yields before finishing evaluation, the thunk will be turned
 * into a `BLACKHOLE` in `ThreadPaused.c:threadPaused`. This function traverses
 * the stack of the yielding thread looking for update frames; when such a
 * frame is encountered, it checks the info table of the updatee and:
 *
 *  - if it is `BLACKHOLE` then thunk has already been claimed for evaluation
 *    by another thread and the yielding thread is instead added to the
 *    `BLACKHOLE`'s blocking queue (see Note [suspend duplicate work] in
 *    `ThreadPaused.c`).
 *
 *  - if not, then it blackholes the thunk as done in eager blackholing (but
 *    using the `BLACKHOLE_info` info table instead of `EAGER_BLACKHOLE_info`).
 *
 * Update frames processed in this manner are rewritten to become
 * `stg_marked_upd_frame`s. The stack traversal continues until a
 * `stg_marked_upd_frame_info` frame is encountered, at which point we know
 * that all subsequent frames have already been processed in a previous yield.
 *
 * The entry code of `stg_upd_frame` is considerably simpler than that of
 * `stg_bh_upd_frame` since we know that the thunk has not accumulated any
 * `BLOCKING_QUEUE`s in need of waking (since it was never blackhole'd). This
 * is itself a small optimisation for the common case of uncontended thunk
 * update. By contrast, the entry code of `stg_marked_upd_frame` is identical
 * to that of `stg_bh_upd_frame` and must deal with waking of blocked threads.
 *
 * See `Note [suspend duplicate work]` in `ThreadPaused.c` for a subtle case
 * involving the interaction between lazy and eager blackholing.
 *
 * See `Note [upd-black-hole]` in `Scav.c` for another subtle case.
 *
 *
 * Blocking on a blackhole'd thunk
 * -------------------------------
 * If another thread `tso_1` enters `thnk` while it is blackholed by `tso_0`,
 * the entry code of `BLACKHOLE` will allocate a `MSG_BLACKHOLE` object
 * `msg_bh_0`. This message will be sent to the capability where the thread
 * owning the thunk resides (see `Messages.c:messageBlackHole`). This
 * capability will allocate a `BLOCKING_QUEUE` object `bq_0` recording the fact
 * that `tso_1` is waiting for the result of `thnk`'s evaluation and link it to
 * `thnk` as follows:
 *
 *         thnk
 *     ┌─►┌───────────────────────┐
 *     │  │ EAGER_BLACKHOLE_info  │
 *     │  ├────────────┬──────────┤
 *     │  │ indirectee │ bq_0     ├──────┐
 *     │  ├────────────┼──────────┤      │
 *     │  │ payload[0] │ fv_0     │      │
 *     │  ├────────────┼──────────┤      │
 *     │  │ payload[1] │ fv_1     │      │
 *     │  └────────────┴──────────┘      │
 *     │                                 │       msg_bh_0
 *     │     ┌───────────────────────────┘  ┌──►┌───────────────────────────┐
 *     │     │                              │   │ MSG_BLACKHOLE_info        │
 *     │     │                              │   ├───────────┬───────────────┤
 *     │     │   bq_0                       │   │ link      │ END_TSO_QUEUE │
 *     │     └─►┌──────────────────────┐    │   ├───────────┼───────────────┤
 *     │        │ BLOCKING_QUEUE_info  │    │   │ result    │ NULL          │
 *     │        ├───────────┬──────────┤    │   ├───────────┼───────────────┤      tso_1
 *     │        │ link      │ NULL     │    │   │ tso       │ tso_1         ├────►┌──────┐
 *     │        ├───────────┼──────────┤    │   └───────────┴───────────────┘     │ ...  │
 *     │        │ queue     │ msg_bh_0 ├────┘                                     └──────┘
 *     │        ├───────────┼──────────┤         tso_0
 *     │        │ owner     │ tso_0    ├───────►┌──────┐
 *     │        ├───────────┼──────────┤        │ ...  │
 *     │        │ bh        │ thnk     ├────┐   └──────┘
 *     │        └───────────┴──────────┘    │
 *     │                                    │
 *     └────────────────────────────────────┘
 *
 * Additionally, the `BLOCKING_QUEUE` is added to the `bq` list of the owning
 * TSO, which collects all blocking queue objects which are blocked on thunks
 * owned by the thread.
 *
 * In addition to this book-keeping, the `MSG_BLACKHOLE` message result in
 * `tso_0` being promoted in its capability's run queue in the hope that
 * `tso_1` and other blocked threads may be unblocked more quickly.
 *
 *
 * Waking up blocking queues
 * -------------------------
 * As noted above, when a thread updates a `BLACKHOLE`'d thunk it may find that
 * some threads have added themselves to the thunk's blocking queue. Naturally,
 * we must ensure that these threads are woken up. However, this gets a bit
 * subtle since multiple threads may have raced to enter the thunk.
 *
 * That is, we may end up in a situation like one of these (TODO audit):
 *
 * ### Race A
 *
 *     Thread 0                      Thread 1                     Thread 2
 *     --------------------------    --------------------------   ----------------------
 *     enter thnk
 *                                   enter thnk
 *     thnk.indirectee := tso_0
 *                                   thnk.indirectee := tso_1
 *     thnk.info := BLACKHOLE
 *                                   thnk.info := BLACKHOLE
 *                                                                enter, block on thnk
 *                                                                send MSG_BLACKHOLE to tso_1->cap
 *     finishes evaluation
 *     thnk.indirectee := result
 *                                   handle MSG_BLACKHOLE
 *                                   add
 *
 * ### Race B
 *
 *     Thread 0                      Thread 1                     Thread 2
 *     --------------------------    --------------------------   ----------------------
 *     enter thnk
 *                                   enter thnk
 *     thnk.indirectee := tso_0
 *                                   thnk.indirectee := tso_1
 *     thnk.info := BLACKHOLE
 *                                   thnk.info := BLACKHOLE
 *                                                                enter, block on thnk
 *                                                                send MSG_BLACKHOLE to tso_1->cap
 *                                   handle MSG_BLACKHOLE
 *                                   add
 *     finishes evaluation
 *     thnk.indirectee := result
 *
 * ### Race C
 *
 *     Thread 0                      Thread 1                     Thread 2
 *     --------------------------    --------------------------   ----------------------
 *     enter thnk
 *                                   enter thnk
 *     thnk.indirectee := tso_0
 *     thnk.info := BLACKHOLE
 *                                                                enter, block on thnk
 *                                                                send MSG_BLACKHOLE to tso_0->cap
 *     handle MSG_BLACKHOLE
 *     thnk.indirectee := new BLOCKING_QUEUE
 *
 *                                   thnk.indirectee := tso_1
 *                                   thnk.info := BLACKHOLE
 *
 *
 * Exception handling
 * ------------------
 * When an exception is thrown to a thread which is evaluating a thunk, it is
 * important that we put things back into a state in which evaluation can
 * be resumed by another thread. This is done by `RaiseAsync.c:raiseAsync`
 * which walks the stack looking for update frames and rewrites the updatees
 * into indirections pointing to an `AP_STACK` closure recording the aborted
 * execution state.
 *
 * See `RaiseAsync.c:raiseAsync` for details.
 *
 *
 * CAFs
 * ----
 * Top-level thunks (CAFs) reuse much of this machinery. The only differences
 * are
 *
 *  - CAF entry ensures mutual exclusion (see `Note [atomic CAF entry]`
 *    in `Storage.c` for why)
 *
 *  - we have a distinct blackhole type, `stg_CAF_BLACKHOLE_info`; it's
 *    not clear that maintaining this distinction from
 *    `stg_EAGER_BLACKHOLE_info` is strictly necessary.
 *
 * See `Note [CAF management]` in `Storage.c` .
 *
 *
 * Memory ordering
 * ---------------
 * The memory orderings necessary for safe concurrent thunk evaluation
 * are rather subtle and described in Note [Heap memory barriers] in `SMP.h`.
 *
 *
 * The uses of `stg_IND`
 * ---------------------
 * As noted above, `stg_IND_info` is not used for thunk evaluation. Instead, it
 * merely serves as a general-purpose indirection in a few miscellaneous cases:
 *
 *  * it is used to "short-out" `BLOCKING_QUEUE`s and `MVAR_TSO_QUEUES` that have
 *    already been woken-up. See Note [BLACKHOLE pointing to IND] in `Evac.c`.
 *
 *  * It is used to perform indirection of selector thunks (see
 *    `Evac.c:unchain_thunk_selectors`).
 *
 */

/* -----------------------------------------------------------------------------
   Updates
   -------------------------------------------------------------------------- */

/* LDV profiling:
 *   After all, we do *NOT* need to call LDV_RECORD_CREATE() for IND
 *   closures because they are inherently used. But, it corrupts
 *   the invariants that every closure keeps its creation time in the profiling
 *  field. So, we call LDV_RECORD_CREATE().
 */

/*
 * We have two versions of this macro (sadly), one for use in C-- code,
 * and the other for C.
 *
 * The and_then argument is a performance hack so that we can paste in
 * the continuation code directly.  It helps shave a couple of
 * instructions off the common case in the update code, which is
 * worthwhile (the update code is often part of the inner loop).
 */
#if defined(CMINUSMINUS)

#define UPDATE_FRAME_FIELDS(w_,p_,info_ptr,ccs,p2,updatee)      \
                 w_ info_ptr,                           \
                 PROF_HDR_FIELDS(w_,ccs,p2)              \
                 p_ updatee

/*
 * Getting the memory barriers correct here is quite tricky. Essentially
 * the write barrier ensures that any writes to the new indirectee are visible
 * before we introduce the indirection.
 * See Note [Heap memory barriers] in SMP.h.
 */
#define updateWithIndirection(p1, p2, and_then) \
    W_ bd;                                                      \
    bd = Bdescr(p1);                                            \
    if (bdescr_gen_no(bd) != 0 :: bits16) {                     \
      IF_NONMOVING_WRITE_BARRIER_ENABLED {                      \
        ccall updateRemembSetPushThunk_(BaseReg, p1 "ptr");     \
      }                                                         \
      recordMutableCap(p1, TO_W_(bdescr_gen_no(bd)));           \
      TICK_UPD_OLD_IND();                                       \
    } else {                                                    \
      TICK_UPD_NEW_IND();                                       \
    }                                                           \
                                                                \
    OVERWRITING_CLOSURE(p1);                                    \
    %release StgInd_indirectee(p1) = p2;                        \
    %release SET_INFO(p1, stg_BLACKHOLE_info);                  \
    LDV_RECORD_CREATE(p1);                                      \
    and_then;

#else /* !CMINUSMINUS */

INLINE_HEADER void updateWithIndirection (Capability *cap,
                                          StgClosure *p1,
                                          StgClosure *p2)
{
    ASSERT( (P_)p1 != (P_)p2 );
    /* not necessarily true: ASSERT( !closure_IND(p1) ); */
    /* occurs in RaiseAsync.c:raiseAsync() */
    /* See Note [Heap memory barriers] in SMP.h */
    bdescr *bd = Bdescr((StgPtr)p1);
    if (bd->gen_no != 0) {
      IF_NONMOVING_WRITE_BARRIER_ENABLED {
          updateRemembSetPushThunk(cap, (StgThunk*)p1);
      }
        recordMutableCap(p1, cap, bd->gen_no);
        TICK_UPD_OLD_IND();
    } else {
        TICK_UPD_NEW_IND();
    }
    OVERWRITING_CLOSURE(p1);
    RELEASE_STORE(&((StgInd *)p1)->indirectee, p2);
    SET_INFO_RELAXED(p1, &stg_BLACKHOLE_info);
    LDV_RECORD_CREATE(p1);
}

#endif /* CMINUSMINUS */

#if !defined(CMINUSMINUS)
#include "EndPrivate.h"
#endif
