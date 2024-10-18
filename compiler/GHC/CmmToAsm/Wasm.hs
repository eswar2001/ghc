{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module GHC.CmmToAsm.Wasm (ncgWasm) where

import Data.ByteString.Builder
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Maybe
import Data.Semigroup
import GHC.Cmm
import GHC.Cmm.ContFlowOpt
import GHC.Cmm.GenericOpt
import GHC.CmmToAsm.Config
import GHC.CmmToAsm.Wasm.Asm
import GHC.CmmToAsm.Wasm.FromCmm
import GHC.CmmToAsm.Wasm.Types
import GHC.StgToCmm.CgUtils (CgStream)
import GHC.Data.Stream (StreamS (..), runStream, liftIO)
import GHC.Driver.DynFlags
import GHC.Platform
import GHC.Prelude
import GHC.Settings
import GHC.Types.Unique.DSM
import GHC.Unit
import GHC.Utils.Logger
import GHC.Utils.Outputable (text)
import System.IO

ncgWasm ::
  NCGConfig ->
  Logger ->
  Platform ->
  ToolSettings ->
  ModLocation ->
  Handle ->
  CgStream RawCmmGroup a ->
  UniqDSMT IO a
ncgWasm ncg_config logger platform ts loc h cmms = do
  (r, s) <- streamCmmGroups ncg_config platform cmms
  outputWasm $ "# " <> string7 (fromJust $ ml_hs_file loc) <> "\n\n"
  -- See Note [WasmTailCall]
  let cfg = (defaultWasmAsmConfig s) { pic = ncgPIC ncg_config, tailcall = doTailCall ts }
  outputWasm $ execWasmAsmM cfg $ asmTellEverything TagI32 s
  pure r
  where
    outputWasm builder = liftIO $ do
      putDumpFileMaybe
        logger
        Opt_D_dump_asm
        "Asm Code"
        FormatASM
        (text . unpack $ toLazyByteString builder)
      hPutBuilder h builder

streamCmmGroups ::
  NCGConfig ->
  Platform ->
  CgStream RawCmmGroup a ->
  UniqDSMT IO (a, WasmCodeGenState 'I32)
streamCmmGroups ncg_config platform cmms = withDUS $ \us -> do
  (r,s) <- go (initialWasmCodeGenState platform us) $ runStream cmms
  return ((r,s), wasmDUniqSupply s)
  where
    go s (Done r) = pure (r, s)
    go s (Effect m) = do
      (a, us') <- runUDSMT (wasmDUniqSupply s) m
      go s{wasmDUniqSupply = us'} a
    go s (Yield decls k) = go (wasmExecM (onCmmGroup $ map opt decls) s) k
      where
        -- Run the generic cmm optimizations like other NCGs, followed
        -- by a late control-flow optimization pass that does shrink
        -- the CFG block count in some cases.
        opt decl = case decl of
          CmmData {} -> decl
          CmmProc {} -> CmmProc info lbl live $ cmmCfgOpts False graph
            where
              (CmmProc info lbl live graph, _) = cmmToCmm ncg_config decl

doTailCall :: ToolSettings -> Bool
doTailCall ts = Option "-mtail-call" `elem` as_args
  where
    (_, as_args) = toolSettings_pgm_a ts
