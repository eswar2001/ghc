{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnliftedFFITypes #-}

module GHC.Exts.DecodeHeap where
import Prelude
import GHC.Exts.Heap.Closures
import GHC.Exts.Heap.ClosureTypes
import GHC.Exts.Heap.Constants
import GHC.Exts.Heap.ProfInfo.Types
#if defined(PROFILING)
import GHC.Exts.Heap.InfoTableProf
#else
import GHC.Exts.Heap.InfoTable
#endif
import GHC.Exts.Heap.Utils
import qualified GHC.Exts.Heap.FFIClosures as FFIClosures
import qualified GHC.Exts.Heap.ProfInfo.PeekProfInfo as PPI

import Control.Monad
import Data.Bits
import Foreign
import GHC.Exts


-- | Convert an unpacked heap object, to a `GenClosure b`. The inputs to this
-- function can be generated from a heap object using `unpackClosure#`.
getClosureDataFromHeapRep :: ByteArray# -> Ptr StgInfoTable -> [b] -> IO (GenClosure b)
getClosureDataFromHeapRep heapRep infoTablePtr pts = do
  itbl <- peekItbl infoTablePtr
  getClosureDataFromHeapRepPrim (dataConNames infoTablePtr) PPI.peekTopCCS itbl heapRep pts

getClosureDataFromHeapRepPrim
    :: IO (String, String, String)
    -- ^ A continuation used to decode the constructor description field,
    -- in ghc-debug this code can lead to segfaults because dataConNames
    -- will dereference a random part of memory.
    -> (Ptr a -> IO (Maybe CostCentreStack))
    -- ^ A continuation which is used to decode a cost centre stack
    -- In ghc-debug, this code will need to call back into the debuggee to
    -- fetch the representation of the CCS before decoding it. Using
    -- `peekTopCCS` for this argument can lead to segfaults in ghc-debug as
    -- the CCS argument will point outside the copied closure.
    -> StgInfoTable
    -- ^ The `StgInfoTable` of the closure, extracted from the heap
    -- representation.
    -> ByteArray#
    -- ^ Heap representation of the closure as returned by `unpackClosure#`.
    -- This includes all of the object including the header, info table
    -- pointer, pointer data, and non-pointer data. The ByteArray# may be
    -- pinned or unpinned.
    -> [b]
    -- ^ Pointers in the payload of the closure, extracted from the heap
    -- representation as returned by `collect_pointers()` in `Heap.c`. The type
    -- `b` is some representation of a pointer e.g. `Any` or `Ptr Any`.
    -> IO (GenClosure b)
    -- ^ Heap representation of the closure.
getClosureDataFromHeapRepPrim getConDesc decodeCCS itbl heapRep pts = do
    let -- heapRep as a list of words.
        rawHeapWords :: [Word]
        rawHeapWords = [W# (indexWordArray# heapRep i) | I# i <- [0.. end] ]
            where
            nelems = I# (sizeofByteArray# heapRep) `div` wORD_SIZE
            end = fromIntegral nelems - 1

        -- Just the payload of rawHeapWords (no header).
        payloadWords :: [Word]
        payloadWords = drop (closureTypeHeaderSize (tipe itbl)) rawHeapWords

        -- The non-pointer words in the payload. Only valid for closures with a
        -- "pointers first" layout. Not valid for bit field layout.
        npts :: [Word]
        npts = drop (closureTypeHeaderSize (tipe itbl) + length pts) rawHeapWords
    case tipe itbl of
        t | t >= CONSTR && t <= CONSTR_NOCAF -> do
            (p, m, n) <- getConDesc
            pure $ ConstrClosure itbl pts npts p m n

        t | t >= THUNK && t <= THUNK_STATIC -> do
            pure $ ThunkClosure itbl pts npts

        THUNK_SELECTOR -> do
            unless (length pts >= 1) $
                fail "Expected at least 1 ptr argument to THUNK_SELECTOR"
            pure $ SelectorClosure itbl (head pts)

        t | t >= FUN && t <= FUN_STATIC -> do
            pure $ FunClosure itbl pts npts

        AP -> do
            unless (length pts >= 1) $
                fail "Expected at least 1 ptr argument to AP"
            -- We expect at least the arity, n_args, and fun fields
            unless (length payloadWords >= 2) $
                fail "Expected at least 2 raw words to AP"
            let splitWord = payloadWords !! 0
            pure $ APClosure itbl
#if defined(WORDS_BIGENDIAN)
                (fromIntegral $ shiftR splitWord (wORD_SIZE_IN_BITS `div` 2))
                (fromIntegral splitWord)
#else
                (fromIntegral splitWord)
                (fromIntegral $ shiftR splitWord (wORD_SIZE_IN_BITS `div` 2))
#endif
                (head pts) (tail pts)

        PAP -> do
            unless (length pts >= 1) $
                fail "Expected at least 1 ptr argument to PAP"
            -- We expect at least the arity, n_args, and fun fields
            unless (length payloadWords >= 2) $
                fail "Expected at least 2 raw words to PAP"
            let splitWord = payloadWords !! 0
            pure $ PAPClosure itbl
#if defined(WORDS_BIGENDIAN)
                (fromIntegral $ shiftR splitWord (wORD_SIZE_IN_BITS `div` 2))
                (fromIntegral splitWord)
#else
                (fromIntegral splitWord)
                (fromIntegral $ shiftR splitWord (wORD_SIZE_IN_BITS `div` 2))
#endif
                (head pts) (tail pts)

        AP_STACK -> do
            unless (length pts >= 1) $
                fail "Expected at least 1 ptr argument to AP_STACK"
            pure $ APStackClosure itbl (head pts) (tail pts)

        IND -> do
            unless (length pts >= 1) $
                fail "Expected at least 1 ptr argument to IND"
            pure $ IndClosure itbl (head pts)

        IND_STATIC -> do
            unless (length pts >= 1) $
                fail "Expected at least 1 ptr argument to IND_STATIC"
            pure $ IndClosure itbl (head pts)

        BLACKHOLE -> do
            unless (length pts >= 1) $
                fail "Expected at least 1 ptr argument to BLACKHOLE"
            pure $ BlackholeClosure itbl (head pts)

        BCO -> do
            unless (length pts >= 3) $
                fail $ "Expected at least 3 ptr argument to BCO, found "
                        ++ show (length pts)
            unless (length payloadWords >= 4) $
                fail $ "Expected at least 4 words to BCO, found "
                        ++ show (length payloadWords)
            let splitWord = payloadWords !! 3
            pure $ BCOClosure itbl (pts !! 0) (pts !! 1) (pts !! 2)
#if defined(WORDS_BIGENDIAN)
                (fromIntegral $ shiftR splitWord (wORD_SIZE_IN_BITS `div` 2))
                (fromIntegral splitWord)
#else
                (fromIntegral splitWord)
                (fromIntegral $ shiftR splitWord (wORD_SIZE_IN_BITS `div` 2))
#endif
                (drop 4 payloadWords)

        ARR_WORDS -> do
            unless (length payloadWords >= 1) $
                fail $ "Expected at least 1 words to ARR_WORDS, found "
                        ++ show (length payloadWords)
            pure $ ArrWordsClosure itbl (head payloadWords) (tail payloadWords)

        t | t >= MUT_ARR_PTRS_CLEAN && t <= MUT_ARR_PTRS_FROZEN_CLEAN -> do
            unless (length payloadWords >= 2) $
                fail $ "Expected at least 2 words to MUT_ARR_PTRS_* "
                        ++ "found " ++ show (length payloadWords)
            pure $ MutArrClosure itbl (payloadWords !! 0) (payloadWords !! 1) pts

        t | t >= SMALL_MUT_ARR_PTRS_CLEAN && t <= SMALL_MUT_ARR_PTRS_FROZEN_CLEAN -> do
            unless (length payloadWords >= 1) $
                fail $ "Expected at least 1 word to SMALL_MUT_ARR_PTRS_* "
                        ++ "found " ++ show (length payloadWords)
            pure $ SmallMutArrClosure itbl (payloadWords !! 0) pts

        t | t == MUT_VAR_CLEAN || t == MUT_VAR_DIRTY -> do
            unless (length pts >= 1) $
                fail $ "Expected at least 1 words to MUT_VAR, found "
                        ++ show (length pts)
            pure $ MutVarClosure itbl (head pts)

        t | t == MVAR_CLEAN || t == MVAR_DIRTY -> do
            unless (length pts >= 3) $
                fail $ "Expected at least 3 ptrs to MVAR, found "
                        ++ show (length pts)
            pure $ MVarClosure itbl (pts !! 0) (pts !! 1) (pts !! 2)

        BLOCKING_QUEUE ->
            pure $ OtherClosure itbl pts rawHeapWords
        --    pure $ BlockingQueueClosure itbl
        --        (pts !! 0) (pts !! 1) (pts !! 2) (pts !! 3)

        --  pure $ OtherClosure itbl pts rawHeapWords
        --
        WEAK -> do
            pure $ WeakClosure
                { info = itbl
                , cfinalizers = pts !! 0
                , key = pts !! 1
                , value = pts !! 2
                , finalizer = pts !! 3
                , weakLink = case drop 4 pts of
                           []  -> Nothing
                           [p] -> Just p
                           _   -> error $ "Expected 4 or 5 words in WEAK, found " ++ show (length pts)
                }
        TSO | [ u_lnk, u_gbl_lnk, tso_stack, u_trec, u_blk_ex, u_bq] <- pts
                -> withArray rawHeapWords (\ptr -> do
                    fields <- FFIClosures.peekTSOFields decodeCCS ptr
                    pure $ TSOClosure
                        { info = itbl
                        , link = u_lnk
                        , global_link = u_gbl_lnk
                        , tsoStack = tso_stack
                        , trec = u_trec
                        , blocked_exceptions = u_blk_ex
                        , bq = u_bq
                        , what_next = FFIClosures.tso_what_next fields
                        , why_blocked = FFIClosures.tso_why_blocked fields
                        , flags = FFIClosures.tso_flags fields
                        , threadId = FFIClosures.tso_threadId fields
                        , saved_errno = FFIClosures.tso_saved_errno fields
                        , tso_dirty = FFIClosures.tso_dirty fields
                        , alloc_limit = FFIClosures.tso_alloc_limit fields
                        , tot_stack_size = FFIClosures.tso_tot_stack_size fields
                        , prof = FFIClosures.tso_prof fields
                        })
            | otherwise
                -> fail $ "Expected 6 ptr arguments to TSO, found "
                        ++ show (length pts)
        STACK
            | [] <- pts
            -> withArray rawHeapWords (\ptr -> do
                            fields <- FFIClosures.peekStackFields ptr
                            pure $ StackClosure
                                { info = itbl
                                , stack_size = FFIClosures.stack_size fields
                                , stack_dirty = FFIClosures.stack_dirty fields
#if __GLASGOW_HASKELL__ >= 811
                                , stack_marking = FFIClosures.stack_marking fields
#endif
                                })
            | otherwise
                -> fail $ "Expected 0 ptr argument to STACK, found "
                    ++ show (length pts)

        _ ->
            pure $ UnsupportedClosure itbl
