{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module GHC.StgToJS.Literal
  ( genLit
  , genStaticLit
  )
where

import GHC.Prelude

import GHC.JS.Syntax
import GHC.JS.Make

import GHC.StgToJS.Types
import GHC.StgToJS.Monad

import GHC.Data.FastString
import GHC.Types.Literal
import GHC.Types.Basic
import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Utils.Outputable
import GHC.Float

import qualified Data.ByteString.Short as Short
import Data.Bits as Bits
import Data.Char (ord)

-- | Generate JS expressions for a Literal
--
-- Literals represented with 2 values:
--  * Addr# (Null and Strings): array and offset
--  * 64-bit values: high 32-bit, low 32-bit
--  * labels: call to h$mkFunctionPtr and 0, or function name and 0
genLit :: HasDebugCallStack => Literal -> G [JExpr]
genLit = \case
  LitChar c     -> return [ toJExpr (ord c) ]
  LitString str ->
    withNewIdent $ \strLit@(TxtI strLitT) ->
      withNewIdent $ \strOff@(TxtI strOffT) -> do
        emitStatic strLitT (StaticUnboxed (StaticUnboxedString str)) Nothing
        emitStatic strOffT (StaticUnboxed (StaticUnboxedStringOffset str)) Nothing
        return [ ValExpr (JVar strLit), ValExpr (JVar strOff) ]
  LitNullAddr              -> return [ null_, ValExpr (JInt 0) ]
  LitNumber nt v           -> case nt of
    LitNumInt     -> return [ toJExpr v ]
    LitNumInt8    -> return [ toJExpr v ]
    LitNumInt16   -> return [ toJExpr v ]
    LitNumInt32   -> return [ toJExpr v ]
    LitNumInt64   -> return [ toJExpr (Bits.shiftR v 32), toJExpr (toSigned v) ]
    LitNumWord    -> return [ toJExpr (toSigned v) ]
    LitNumWord8   -> return [ toJExpr (toSigned v) ]
    LitNumWord16  -> return [ toJExpr (toSigned v) ]
    LitNumWord32  -> return [ toJExpr (toSigned v) ]
    LitNumWord64  -> return [ toJExpr (toSigned (Bits.shiftR v 32)), toJExpr (toSigned v) ]
    LitNumBigNat  -> panic "genLit: unexpected BigNat that should have been removed in CorePrep"
  LitFloat r               -> return [ toJExpr (r2f r) ]
  LitDouble r              -> return [ toJExpr (r2d r) ]
  LitLabel name _size fod
    | fod == IsFunction      -> return [ ApplExpr (var "h$mkFunctionPtr")
                                                  [var (mkFastString $ "h$" ++ unpackFS name)]
                                       , ValExpr (JInt 0)
                                       ]
    | otherwise              -> return [ toJExpr (TxtI . mkFastString $ "h$" ++ unpackFS name)
                                       , ValExpr (JInt 0)
                                       ]
  LitRubbish _rep -> return [ null_ ]

-- | generate a literal for the static init tables
genStaticLit :: Literal -> G [StaticLit]
genStaticLit = \case
  LitChar c                -> return [ IntLit (fromIntegral $ ord c) ]
  LitString str
    | True                 -> return [ StringLit (mkFastStringByteString str), IntLit 0]
    -- FIXME: documentation for LitString says it's always UTF8 encoded but it's
    -- not true (e.g. for embedded files).
    --  1) We should add a decoding function that detects errors in
    --  GHC.Utils.Encoding
    --  2) We should perhaps add a different LitBin constructor that would
    --  benefit other backends?
    -- \|  invalid UTF8         -> return [ BinLit str, IntLit 0]
  LitNullAddr              -> return [ NullLit, IntLit 0 ]
  LitNumber LitNumInt i    -> return [ IntLit (fromIntegral i) ]
  LitNumber LitNumInt8 i   -> return [ IntLit (fromIntegral i) ]
  LitNumber LitNumInt16 i  -> return [ IntLit (fromIntegral i) ]
  LitNumber LitNumInt32 i  -> return [ IntLit (fromIntegral i) ]
  LitNumber LitNumInt64 i  -> return [ IntLit (i `Bits.shiftR` 32), IntLit (toSigned i) ]
  LitNumber LitNumWord w   -> return [ IntLit (toSigned w) ]
  LitNumber LitNumWord8  w -> return [ IntLit (toSigned w) ]
  LitNumber LitNumWord16 w -> return [ IntLit (toSigned w) ]
  LitNumber LitNumWord32 w -> return [ IntLit (toSigned w) ]
  LitNumber LitNumWord64 w -> return [ IntLit (toSigned (w `Bits.shiftR` 32)), IntLit (toSigned w) ]
  LitFloat r               -> return [ DoubleLit . SaneDouble . r2f $ r ]
  LitDouble r              -> return [ DoubleLit . SaneDouble . r2d $ r ]
  LitLabel name _size fod  -> return [ LabelLit (fod == IsFunction) (mkFastString $ "h$" ++ unpackFS name)
                                     , IntLit 0 ]
  -- FIXME: handle other LitNumbers, LitRubbish, etc.
  l -> pprPanic "genStaticLit" (ppr l)

-- make a signed 32 bit int from this unsigned one, lower 32 bits
toSigned :: Integer -> Integer
toSigned i | Bits.testBit i 31 = Bits.complement (0x7FFFFFFF `Bits.xor` (i Bits..&. 0x7FFFFFFF))
           | otherwise         = i Bits..&. 0xFFFFFFFF

r2d :: Rational -> Double
r2d = realToFrac

r2f :: Rational -> Double
r2f = float2Double . realToFrac
