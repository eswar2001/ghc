-- TODO: Can orphanage be avoided?
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Native code generator for RISC-V 64bit architecture
module GHC.CmmToAsm.RISCV64
   ( ncgRISCV64
   )
where
import GHC.Prelude

import GHC.CmmToAsm.Instr
import GHC.CmmToAsm.Monad
import GHC.CmmToAsm.Config
import GHC.CmmToAsm.Types

import qualified GHC.CmmToAsm.RISCV64.Instr   as RISCV64
import qualified GHC.CmmToAsm.RISCV64.Ppr     as RISCV64
import qualified GHC.CmmToAsm.RISCV64.CodeGen as RISCV64
import qualified GHC.CmmToAsm.RISCV64.Regs    as RISCV64
import qualified GHC.CmmToAsm.RISCV64.RegInfo as RISCV64

ncgRISCV64 :: NCGConfig -> NcgImpl RawCmmStatics RISCV64.Instr RISCV64.JumpDest
ncgRISCV64 config = NcgImpl
   { ncgConfig                 = config
   , cmmTopCodeGen             = RISCV64.cmmTopCodeGen
   , generateJumpTableForInstr = RISCV64.generateJumpTableForInstr
   , getJumpDestBlockId        = RISCV64.getJumpDestBlockId
   , canShortcut               = RISCV64.canShortcut
   , shortcutStatics           = RISCV64.shortcutStatics
   , shortcutJump              = RISCV64.shortcutJump
   , pprNatCmmDeclH            = RISCV64.pprNatCmmDeclH
   , pprNatCmmDeclS            = RISCV64.pprNatCmmDeclS
   , maxSpillSlots             = RISCV64.maxSpillSlots
   , allocatableRegs           = RISCV64.allocatableRegs
   , ncgAllocMoreStack         = RISCV64.allocMoreStack
   , ncgMakeFarBranches        = RISCV64.makeFarBranches
   , extractUnwindPoints       = const []
   , invertCondBranches        = \_ _ -> id
   }

-- | Instruction instance for RISC-V 64bit
instance Instruction RISCV64.Instr where
   regUsageOfInstr     = RISCV64.regUsageOfInstr
   patchRegsOfInstr    = RISCV64.patchRegsOfInstr
   isJumpishInstr      = RISCV64.isJumpishInstr
   jumpDestsOfInstr    = RISCV64.jumpDestsOfInstr
   patchJumpInstr      = RISCV64.patchJumpInstr
   mkSpillInstr        = RISCV64.mkSpillInstr
   mkLoadInstr         = RISCV64.mkLoadInstr
   takeDeltaInstr      = RISCV64.takeDeltaInstr
   isMetaInstr         = RISCV64.isMetaInstr
   mkRegRegMoveInstr   = RISCV64.mkRegRegMoveInstr
   takeRegRegMoveInstr = RISCV64.takeRegRegMoveInstr
   mkJumpInstr         = RISCV64.mkJumpInstr
   mkStackAllocInstr   = RISCV64.mkStackAllocInstr
   mkStackDeallocInstr = RISCV64.mkStackDeallocInstr
   pprInstr            = RISCV64.pprInstr
   mkComment           = pure . RISCV64.COMMENT
