module Language.Assembly.X86.Print where

import Data.Char
import Data.Int
import Data.List
import Data.Word

import Numeric

import Language.Assembly.X86.Instruction

-- Display an opcode in lower case.

showOp :: Opcode -> String
showOp = (map toLower) . show

-- Show an operand in AT&T style.

showAttOps (OpImm w) = showImm w
showAttOps (OpAddr w _) = showAddr w
showAttOps (OpReg s num) = "%" ++ s
showAttOps (OpFPReg 0) = "%st"
showAttOps (OpFPReg i) = "%st(" ++ show i ++ ")"
showAttOps (OpInd s _) = "(%" ++ s ++ ")"
showAttOps (OpIndDisp s disp _) = show disp ++ "(%" ++ s ++ ")"
showAttOps (OpBaseIndex b i s _) = "(%" ++ b ++ ",%" ++ i ++ "," ++ show s ++ ")"
showAttOps (OpIndexDisp i s disp _) = show disp ++ "(%" ++ i ++ "," ++
  show s ++ ")"
showAttOps (OpBaseIndexDisp b i s disp _) = show disp ++ "(%" ++ b ++ ",%" ++
  i ++ "," ++ show s ++ ")"

-- Show an operand in Intel style.

showIntelOps opsize (OpImm w) = showIntelImm w
showIntelOps opsize (OpAddr w sz) = opInd sz ++ "[" ++ showIntelAddr w ++ "]"
showIntelOps opsize (OpReg s num) = s
showIntelOps opsize (OpFPReg 0) = "st"
showIntelOps opsize (OpFPReg i) = "st(" ++ show i ++ ")"
showIntelOps opsize (OpInd s sz) = opInd sz ++ "[" ++ s ++ "]"
showIntelOps opsize (OpIndDisp s disp sz) =
    opInd sz ++ "[" ++ s ++
       (if disp < 0 then "" else "+") ++ show disp ++ "]"
showIntelOps opsize (OpBaseIndex b i s sz) =
    opInd sz ++ "[" ++ b ++ "+" ++ i ++ "*" ++ show s ++ "]"
showIntelOps opsize (OpIndexDisp i s disp sz) =
    opInd sz ++ "[" ++ i ++ "*" ++ show s ++
       (if disp < 0 then "" else "+") ++ show disp ++ "]"
showIntelOps opsize (OpBaseIndexDisp b i s disp sz) =
    opInd sz ++ "[" ++ b ++ "+" ++ i ++ "*" ++ show s ++
       (if disp < 0 then "" else "+") ++
      show disp ++ "]"
opInd OPNONE = ""
opInd OP8 = "byte ptr "
opInd OP16 = "word ptr "
opInd OP32 = "dword ptr "
opInd OPF32 = "dword ptr "
opInd OP64 = "qword ptr "
opInd OPF64 = "qword ptr "
opInd OPF80 = "tbyte ptr "
opInd OP128 = "dqword ptr "


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
showIntel (BadInstruction b desc pos bytes) =
    showPosBytes pos bytes ++
    "(" ++ desc ++ ", byte=" ++ show b ++ ")"
showIntel (PseudoInstruction pos s) =
    hex32 pos ++ "                          " ++ s
showIntel (Instruction op opsize [] pos bytes) =
    showPosBytes pos bytes ++
       showOp op
showIntel (Instruction op opsize ops pos bytes) =
    showPosBytes pos bytes ++
        enlarge (showOp op) 6 ++ " " ++
       concat (intersperse "," (map (showIntelOps opsize) ops))

-- | Show an instruction in AT&T style.

showAtt :: Instruction -> [Char]
showAtt (BadInstruction b desc pos bytes) =
    showPosBytes pos bytes ++
       "(" ++ desc ++ ", byte=" ++ show b ++ ")"
showAtt (PseudoInstruction pos s) =
    hex32 pos ++ "                          " ++ s
showAtt (Instruction op opsize [] pos bytes) =
    showPosBytes pos bytes ++
       showOp op ++ showInstrSuffix [] opsize
showAtt (Instruction op opsize ops pos bytes) =
    showPosBytes pos bytes ++
       enlarge (showOp op ++ showInstrSuffix ops opsize) 6 ++ " " ++
       concat (intersperse "," (map showAttOps (reverse ops)))

showPosBytes pos bytes =
    hex32 pos ++ "  " ++
      enlarge (concat (intersperse " " (map hex8 bytes))) 30

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

showInstrSuffix [] sz                                     = opSizeSuffix sz
showInstrSuffix ((OpImm _) : os) s                        = showInstrSuffix os s
--showInstrSuffix ((OpReg _ _) : []) s                      = ""
showInstrSuffix ((OpReg _ _) : os) s                      = showInstrSuffix os OPNONE
showInstrSuffix ((OpFPReg _) : os) s                      = showInstrSuffix os s
showInstrSuffix ((OpAddr _ OPNONE) : os) s                = showInstrSuffix os s
showInstrSuffix ((OpAddr _ sz) : os) s                    = opSizeSuffix sz
showInstrSuffix ((OpInd _ OPNONE) : os) s                 = showInstrSuffix os s
showInstrSuffix ((OpInd _ sz) : os) s                     = opSizeSuffix sz
showInstrSuffix ((OpIndDisp _ _ OPNONE) : os) s           = showInstrSuffix os s
showInstrSuffix ((OpIndDisp _ _ sz) : os) s               = opSizeSuffix sz
showInstrSuffix ((OpBaseIndex _ _ _ OPNONE) : os) s       = showInstrSuffix os s
showInstrSuffix ((OpBaseIndex _ _ _ sz) : os) s           = opSizeSuffix sz
showInstrSuffix ((OpIndexDisp _ _ _ OPNONE) : os) s       = showInstrSuffix os s
showInstrSuffix ((OpIndexDisp _ _ _ sz) : os) s           = opSizeSuffix sz
showInstrSuffix ((OpBaseIndexDisp _ _ _ _ OPNONE) : os) s = showInstrSuffix os s
showInstrSuffix ((OpBaseIndexDisp _ _ _ _ sz) : os) s     = opSizeSuffix sz

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
