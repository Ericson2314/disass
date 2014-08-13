--------------------------------------------------------------------------
-- |
-- Module      :  Language.Assembly.X86
-- Copyright   :  (c) Martin Grabmueller and Dirk Kleeblatt
-- License     :  BSD3
--
-- Maintainer  :  martin@grabmueller.de,klee@cs.tu-berlin.de
-- Stability   :  provisional
-- Portability :  portable
--
-- Disassembler for x86 machine code.
--
-- This is a disassembler for object code for the x86 architecture.
-- It provides functions for disassembling byte arrays, byte lists and
-- memory blocks containing raw binary code.
--
-- Features:
--
-- - Disassembles memory blocks, lists or arrays of bytes into lists of
--   instructions.
--
-- - Abstract instructions provide as much information as possible about
--   opcodes, addressing modes or operand sizes, allowing for detailed
--   output.
--
-- - Provides functions for displaying instructions in Intel or AT&T
--   style (like the GNU tools)
--
-- Differences to GNU tools, like gdb or objdump:
--
-- - Displacements are shown in decimal, with sign if negative.
--
-- Missing:
--
-- - LOCK and repeat prefixes are recognized, but not contained in the
--   opcodes of instructions.
--
-- - Support for 16-bit addressing modes.  Could be added when needed.
--
-- - Complete disassembly of all 64-bit instructions.  I have tried to
--   disassemble them properly but have been limited to the information
--   in the docs, because I have no 64-bit machine to test on.  This will
--   probably change when I get GNU as to produce 64-bit object files.
--
-- - Not all MMX and SSE/SSE2/SSE3 instructions are decoded yet.  This is
--   just a matter of missing time.
--
-- - segment override prefixes are decoded, but not appended to memory
--   references
--
-- On the implementation:
--
-- This disassembler uses the Parsec parser combinators, working on byte
-- lists.  This proved to be very convenient, as the combinators keep
-- track of the current position, etc.
--------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.Assembly.X86 (
  -- * Types
  Opcode,
  Operand(..),
  InstrOperandSize(..),
  Instruction(..),
  ShowStyle(..),
  Config(..),

  -- * Functions
  disassembleBlock,
  disassembleList,
  disassembleArray,
  disassembleFile,
  disassembleBlockWithConfig,
  disassembleListWithConfig,
  disassembleArrayWithConfig,
  disassembleFileWithConfig,

  showIntel,
  showAtt,

  defaultConfig
  ) where

import           Data.Array.IArray
import           Data.Char

import           Foreign

import           Language.Assembly.X86.Instruction
import           Language.Assembly.X86.Parse
import           Language.Assembly.X86.Print

import           Text.Parsec  hiding (many, (<|>))

-- | Disassemble a block of memory.  Starting at the location
-- pointed to by the given pointer, the given number of bytes are
-- disassembled.

disassembleBlock :: Ptr Word8 -> Int -> IO (Either ParseError [Instruction])
disassembleBlock ptr len =
    disassembleBlockWithConfig defaultConfig{confStartAddr = fromIntegral (minusPtr ptr nullPtr)}
                               ptr len

disassembleBlockWithConfig :: Config -> Ptr Word8 -> Int -> IO (Either ParseError [Instruction])
disassembleBlockWithConfig config ptr len =
  do l <- toList ptr len 0 []
     parseInstructions (configToState config) (reverse l)
  where toList :: (Ptr Word8) -> Int -> Int -> [Word8] -> IO [Word8]
        toList ptr len idx acc
          | idx <  len = do p <- peekByteOff ptr idx
                            toList ptr len (idx + 1) (p : acc)
                            --return (p : r)
          | idx >= len = return acc

-- | Disassemble the contents of the given array.

disassembleArray :: (Monad m, IArray a Word8, Ix i) =>
                    a i Word8 -> m (Either ParseError [Instruction])
disassembleArray arr = disassembleArrayWithConfig defaultConfig arr

disassembleArrayWithConfig :: (Monad m, IArray a Word8, Ix i) => Config ->
                             a i Word8 -> m (Either ParseError [Instruction])
disassembleArrayWithConfig config arr = parseInstructions (configToState config) l
  where l = elems arr

-- | Disassemble the contents of the given list.

disassembleList :: (Monad m) =>
                   [Word8] -> m (Either ParseError [Instruction])
disassembleList ls = disassembleListWithConfig defaultConfig ls

disassembleListWithConfig :: (Monad m) => Config ->
                   [Word8] -> m (Either ParseError [Instruction])
disassembleListWithConfig config ls =
    parseInstructions (configToState config) ls

disassembleFile :: FilePath -> IO (Either ParseError [Instruction])
disassembleFile filename = disassembleFileWithConfig defaultConfig filename

disassembleFileWithConfig
  :: Config -> FilePath -> IO (Either ParseError [Instruction])
disassembleFileWithConfig config filename = do
  l <- readFile filename
  parseInstructions (configToState config) (map (fromIntegral . ord) l)

instrToString :: [Instruction] -> ShowStyle -> [[Char]]
instrToString insts style =
  map showInstr insts
 where
 showInstr = case style of
          IntelStyle -> showIntel
          AttStyle -> showAtt

-- | Test function for disassembling the contents of a binary file and
-- displaying it in the provided style ("IntelStyle" or "AttStyle").

testFile :: FilePath -> ShowStyle -> IO ()
testFile fname style = do
  l <- readFile fname
  i <- parseInstructions defaultState (map (fromIntegral . ord) l)
  case i of
    Left err -> putStrLn (show err)
    Right i' -> mapM_ (putStrLn . showInstr) i'
 where
 showInstr = case style of
          IntelStyle -> showIntel
          AttStyle -> showAtt
