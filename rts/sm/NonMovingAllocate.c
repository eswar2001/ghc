/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2018
 *
 * Non-moving garbage collector and allocator
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "RtsUtils.h"
#include "Storage.h"
#include "GCThread.h"
#include "GCTDecl.h"
#include "GCUtils.h"
#include "Capability.h"
#include "NonMovingAllocate.h"

static inline unsigned long log2_ceil(unsigned long x);
static struct NonmovingSegment *nonmovingAllocSegment(uint32_t node);
static void nonmovingClearBitmap(struct NonmovingSegment *seg);
static void nonmovingInitSegment(struct NonmovingSegment *seg, uint8_t log_block_size);
static bool advance_next_free(struct NonmovingSegment *seg, const unsigned int blk_count);
static struct NonmovingSegment *nonmovingPopFreeSegment(void);
static struct NonmovingSegment *pop_active_segment(struct NonmovingAllocator *alloca);

static inline unsigned long log2_ceil(unsigned long x)
{
    return (sizeof(unsigned long)*8) - __builtin_clzl(x-1);
}

/*
 * Request a fresh segment from the free segment list or allocate one of the
 * given node.
 *
 * Caller must hold SM_MUTEX (although we take the gc_alloc_block_sync spinlock
 * under the assumption that we are in a GC context).
 */
static struct NonmovingSegment *nonmovingAllocSegment(uint32_t node)
{
    // First try taking something off of the free list
    struct NonmovingSegment *ret;
    ret = nonmovingPopFreeSegment();

    // Nothing in the free list, allocate a new segment...
    if (ret == NULL) {
        // Take gc spinlock: another thread may be scavenging a moving
        // generation and call `todo_block_full`
        ACQUIRE_ALLOC_BLOCK_SPIN_LOCK();
        bdescr *bd = allocAlignedGroupOnNode(node, NONMOVING_SEGMENT_BLOCKS);
        // See Note [Live data accounting in nonmoving collector].
        oldest_gen->n_blocks += bd->blocks;
        oldest_gen->n_words  += BLOCK_SIZE_W * bd->blocks;
        RELEASE_ALLOC_BLOCK_SPIN_LOCK();

        for (StgWord32 i = 0; i < bd->blocks; ++i) {
            initBdescr(&bd[i], oldest_gen, oldest_gen);
            bd[i].flags = BF_NONMOVING;
        }
        ret = (struct NonmovingSegment *)bd->start;
    }

    // Check alignment
    ASSERT(((uintptr_t)ret % NONMOVING_SEGMENT_SIZE) == 0);
    return ret;
}

static void nonmovingClearBitmap(struct NonmovingSegment *seg)
{
    unsigned int n = nonmovingSegmentBlockCount(seg);
    memset(seg->bitmap, 0, n);
}

static void nonmovingInitSegment(struct NonmovingSegment *seg, uint8_t log_block_size)
{
    bdescr *bd = Bdescr((P_) seg);
    seg->link = NULL;
    seg->todo_link = NULL;
    seg->next_free = 0;
    SET_SEGMENT_STATE(seg, FREE);
    bd->nonmoving_segment.log_block_size = log_block_size;
    bd->nonmoving_segment.next_free_snap = 0;
    bd->u.scan = nonmovingSegmentGetBlock(seg, 0);
    nonmovingClearBitmap(seg);
}

/* Initialize a new capability. Caller must hold SM_LOCK */
void nonmovingInitCapability(Capability *cap)
{
    // Initialize current segment array
    struct NonmovingSegment **segs =
        stgMallocBytes(sizeof(struct NonmovingSegment*) * NONMOVING_ALLOCA_CNT, "current segment array");
    for (unsigned int i = 0; i < NONMOVING_ALLOCA_CNT; i++) {
        segs[i] = nonmovingAllocSegment(cap->node);
        nonmovingInitSegment(segs[i], NONMOVING_ALLOCA0 + i);
        SET_SEGMENT_STATE(segs[i], CURRENT);
    }
    cap->current_segments = segs;

    // Initialize update remembered set
    cap->upd_rem_set.queue.blocks = NULL;
    nonmovingInitUpdRemSet(&cap->upd_rem_set);
}

// Advance a segment's next_free pointer. Returns true if segment if full.
static bool advance_next_free(struct NonmovingSegment *seg, const unsigned int blk_count)
{
    const uint8_t *bitmap = seg->bitmap;
    ASSERT(blk_count == nonmovingSegmentBlockCount(seg));
#if defined(NAIVE_ADVANCE_FREE)
    // reference implementation
    for (unsigned int i = seg->next_free+1; i < blk_count; i++) {
        if (!bitmap[i]) {
            seg->next_free = i;
            return false;
        }
    }
    seg->next_free = blk_count;
    return true;
#else
    const uint8_t *c = memchr(&bitmap[seg->next_free+1], 0, blk_count - seg->next_free - 1);
    if (c == NULL) {
        seg->next_free = blk_count;
        return true;
    } else {
        seg->next_free = c - bitmap;
        return false;
    }
#endif
}

static struct NonmovingSegment *nonmovingPopFreeSegment(void)
{
    while (true) {
        struct NonmovingSegment *seg = ACQUIRE_LOAD(&nonmovingHeap.free);
        if (seg == NULL) {
            return NULL;
        }
        if (cas((StgVolatilePtr) &nonmovingHeap.free,
                (StgWord) seg,
                (StgWord) seg->link) == (StgWord) seg) {
            __sync_sub_and_fetch(&nonmovingHeap.n_free, 1);
            return seg;
        }
    }
}

static struct NonmovingSegment *pop_active_segment(struct NonmovingAllocator *alloca)
{
    while (true) {
        // Synchronizes with CAS in nonmovingPushActiveSegment
        struct NonmovingSegment *seg = ACQUIRE_LOAD(&alloca->active);
        if (seg == NULL) {
            return NULL;
        }
        struct NonmovingSegment *next = RELAXED_LOAD(&seg->link);
        if (cas((StgVolatilePtr) &alloca->active,
                (StgWord) seg,
                (StgWord) next) == (StgWord) seg) {
            return seg;
        }
    }
}

/* Allocate a block in the nonmoving heap. Caller must hold SM_MUTEX. sz is in words */
GNUC_ATTR_HOT
void *nonmovingAllocate(Capability *cap, StgWord sz)
{
    unsigned int log_block_size = log2_ceil(sz * sizeof(StgWord));
    unsigned int block_count = nonmovingBlockCountFromSize(log_block_size);

    // The max we ever allocate is 3276 bytes (anything larger is a large
    // object and not moved) which is covered by allocator 9.
    ASSERT(log_block_size < NONMOVING_ALLOCA0 + NONMOVING_ALLOCA_CNT);

    unsigned int alloca_idx = log_block_size - NONMOVING_ALLOCA0;
    struct NonmovingAllocator *alloca = &nonmovingHeap.allocators[alloca_idx];

    // Allocate into current segment
    struct NonmovingSegment *current = cap->current_segments[alloca_idx];
    ASSERT(current); // current is never NULL
    void *ret = nonmovingSegmentGetBlock_(current, log_block_size, current->next_free);
    ASSERT(GET_CLOSURE_TAG(ret) == 0); // check alignment

    // Advance the current segment's next_free or allocate a new segment if full
    bool full = advance_next_free(current, block_count);
    if (full) {
        // Current segment is full: update live data estimate link it to
        // filled, take an active segment if one exists, otherwise allocate a
        // new segment.

        // Update live data estimate.
        // See Note [Live data accounting in nonmoving collector].
        unsigned int new_blocks = block_count - nonmovingSegmentInfo(current)->next_free_snap;
        unsigned int block_size = 1 << log_block_size;
        atomic_inc(&oldest_gen->live_estimate, new_blocks * block_size / sizeof(W_));

        // push the current segment to the filled list
        nonmovingPushFilledSegment(current);

        // first look for a new segment in the active list
        struct NonmovingSegment *new_current = pop_active_segment(alloca);

        // there are no active segments, allocate new segment
        if (new_current == NULL) {
            new_current = nonmovingAllocSegment(cap->node);
            nonmovingInitSegment(new_current, log_block_size);
        }

        // make it current
        new_current->link = NULL;
        SET_SEGMENT_STATE(new_current, CURRENT);
        cap->current_segments[alloca_idx] = new_current;
    }

    return ret;
}
