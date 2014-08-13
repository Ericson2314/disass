{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.Assembly.X86.Print where

import Data.Char

import Data.List
import Data.Word

import Numeric

import Language.Assembly.X86.Instruction

-- Display an opcode in lower case.

showOp :: Opcode -> String
showOp = (map toLower) . show

-- Show an operand in AT&T style.

showAttOps = \case
  (OpImm w)                      -> showImm w
  (OpAddr w _)                   -> showAddr w
  (OpReg s num)                  -> "%" ++ s
  (OpFPReg 0)                    -> "%st"
  (OpFPReg i)                    -> "%st(" ++ show i ++ ")"
  (OpInd s _)                    -> "(%" ++ s ++ ")"
  (OpIndDisp s disp _)           -> show disp ++ "(%" ++ s ++ ")"
  (OpBaseIndex b i s _)          -> "(%" ++ b ++ ",%" ++ i ++ "," ++ show s ++ ")"
  (OpIndexDisp i s disp _)       -> show disp ++ "(%" ++ i ++ "," ++ show s ++ ")"
  (OpBaseIndexDisp b i s disp _) -> show disp ++ "(%" ++ b ++ ",%" ++ i ++ "," ++ show s ++ ")"

-- Show an operand in Intel style.

showIntelOps opsize = \case
  (OpImm w)                       -> showIntelImm w
  (OpAddr w sz)                   -> opInd sz ++ "[" ++ showIntelAddr w ++ "]"
  (OpReg s num)                   -> s
  (OpFPReg 0)                     -> "st"
  (OpFPReg i)                     -> "st(" ++ show i ++ ")"
  (OpInd s sz)                    -> opInd sz ++ "[" ++ s ++ "]"
  (OpIndDisp s disp sz)           -> opInd sz ++ "[" ++ s ++ (if disp < 0 then "" else "+") ++ show disp ++ "]"
  (OpBaseIndex b i s sz)          -> opInd sz ++ "[" ++ b ++ "+" ++ i ++ "*" ++ show s ++ "]"
  (OpIndexDisp i s disp sz)       -> opInd sz ++ "[" ++ i ++ "*" ++ show s ++ (if disp < 0 then "" else "+") ++ show disp ++ "]"
  (OpBaseIndexDisp b i s disp sz) -> opInd sz ++ "[" ++ b ++ "+" ++ i ++ "*" ++ show s ++ (if disp < 0 then "" else "+") ++ show disp ++ "]"

opInd = \case
  OPNONE -> ""
  OP8    -> "byte ptr "
  OP16   -> "word ptr "
  OP32   -> "dword ptr "
  OPF32  -> "dword ptr "
  OP64   -> "qword ptr "
  OPF64  -> "qword ptr "
  OPF80  -> "tbyte ptr "
  OP128  -> "dqword ptr "


instance Show Instruction where
  show = showIntel

-- Show an integer as an 8-digit hexadecimal number with leading zeroes.

hex32 :: Int -> String
hex32 i = take (8 - length s) (repeat '0') ++ s
  where w :: Word32
        w = fromIntegral i
        s = showHex w ""

-- Show a byte as an 2-digit hexadecimal number with leading zeroes.

hex8 :: Word8 -> String
hex8 i = take (2 - length s) ['0','0'] ++ s
  where s = showHex i ""


-- | Instructions can be displayed either in Intel or AT&T style (like in
-- GNU tools).
--
-- Intel style:
--
-- - Destination operand comes first, source second.
--
-- - No register or immediate prefixes.
--
-- - Memory operands are annotated with operand size.
--
-- - Hexadecimal numbers are suffixed with @H@ and prefixed with @0@ if
--   necessary.
--
-- AT&T style:
--
-- - Source operand comes first, destination second.
--
-- - Register names are prefixes with @%@.
--
-- - Immediates are prefixed with @$@.
--
-- - Hexadecimal numbers are prefixes with @0x@
--
-- - Opcodes are suffixed with operand size, when ambiguous otherwise.
data ShowStyle = IntelStyle             -- ^ Show in Intel style
               | AttStyle              -- ^ Show in AT&T style

-- | Show an instruction in Intel style.

showIntel :: Instruction -> [Char]
showIntel  = \case
  (BadInstruction b desc pos bytes)     -> showPosBytes pos bytes ++ "(" ++ desc ++ ", byte=" ++ show b ++ ")"
  (PseudoInstruction pos s)             -> hex32 pos ++ "                          " ++ s
  (Instruction op opsize [] pos bytes)  -> showPosBytes pos bytes ++ showOp op
  (Instruction op opsize ops pos bytes) -> showPosBytes pos bytes ++ enlarge (showOp op) 6 ++ " " ++ concat (intersperse "," (map (showIntelOps opsize) ops))

-- | Show an instruction in AT&T style.

showAtt :: Instruction -> [Char]
showAtt = \case
  (BadInstruction b desc pos bytes)     -> showPosBytes pos bytes ++ "(" ++ desc ++ ", byte=" ++ show b ++ ")"
  (PseudoInstruction pos s)             -> hex32 pos ++ "                          " ++ s
  (Instruction op opsize [] pos bytes)  -> showPosBytes pos bytes ++ showOp op ++ showInstrSuffix [] opsize
  (Instruction op opsize ops pos bytes) -> showPosBytes pos bytes ++ enlarge (showOp op ++ showInstrSuffix ops opsize) 6 ++ " " ++ concat (intersperse "," (map showAttOps (reverse ops)))

showPosBytes pos bytes = hex32 pos ++ "  " ++ enlarge (concat (intersperse " " (map hex8 bytes))) 30

enlarge s i = s ++ take (i - length s) (repeat ' ')

opSizeSuffix OPNONE = ""
opSizeSuffix OP8    = "b"
opSizeSuffix OP16   = "w"
opSizeSuffix OP32   = "l"
opSizeSuffix OP64   = "q"
opSizeSuffix OP128  = "dq"
opSizeSuffix OPF32  = "s"
opSizeSuffix OPF64  = "l"
opSizeSuffix OPF80  = "t"

showInstrSuffix q_ s = case q_ of
  []                                      -> opSizeSuffix s
  ((OpImm _) : os)                        -> showInstrSuffix os s
-- ((OpReg _ _) : [])                      -> ""
  ((OpReg _ _) : os)                      -> showInstrSuffix os OPNONE
  ((OpFPReg _) : os)                      -> showInstrSuffix os s
  ((OpAddr _ OPNONE) : os)                -> showInstrSuffix os s
  ((OpAddr _ sz) : os)                    -> opSizeSuffix sz
  ((OpInd _ OPNONE) : os)                 -> showInstrSuffix os s
  ((OpInd _ sz) : os)                     -> opSizeSuffix sz
  ((OpIndDisp _ _ OPNONE) : os)           -> showInstrSuffix os s
  ((OpIndDisp _ _ sz) : os)               -> opSizeSuffix sz
  ((OpBaseIndex _ _ _ OPNONE) : os)       -> showInstrSuffix os s
  ((OpBaseIndex _ _ _ sz) : os)           -> opSizeSuffix sz
  ((OpIndexDisp _ _ _ OPNONE) : os)       -> showInstrSuffix os s
  ((OpIndexDisp _ _ _ sz) : os)           -> opSizeSuffix sz
  ((OpBaseIndexDisp _ _ _ _ OPNONE) : os) -> showInstrSuffix os s
  ((OpBaseIndexDisp _ _ _ _ sz) : os)     -> opSizeSuffix sz

-- showInstrOperandSize ops OPNONE | noRegop ops = ""
-- showInstrOperandSize ops OP8 | noRegop ops = "b"
-- showInstrOperandSize ops OP16 | noRegop ops = "w"
-- showInstrOperandSize ops OP32 | noRegop ops = "l"
-- showInstrOperandSize ops OPF32 | noRegop ops = "s"
-- showInstrOperandSize ops OP64 | noRegop ops = "q"
-- showInstrOperandSize ops OPF64 | noRegop ops = "l"
-- showInstrOperandSize ops OPF80 | noRegop ops = "e"
-- showInstrOperandSize ops OP128 | noRegop ops = ""
-- showInstrOperandSize _ _ = ""

-- noRegop ops = null (filter isRegop ops)
-- isRegop (OpReg _ _) = True
-- isRegop _ = False

-- Show an immediate value in hexadecimal.

showImm :: Word32 -> String
showImm i = "$0x" ++ showHex i ""

showIntelImm :: Word32 -> String
showIntelImm i = (if isDigit f then "" else "0") ++ h
  where h = showHex i "H"
        (f:_) = h

-- Show an address in hexadecimal.

showAddr i = "0x" ++ showHex w ""
  where w :: Word32
        w = fromIntegral i

showIntelAddr i = (if isDigit f then "" else "0") ++ h
  where w :: Word32
        w = fromIntegral i
        h = showHex w "H"
        (f:_) = h
