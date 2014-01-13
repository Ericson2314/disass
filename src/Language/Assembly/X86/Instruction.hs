module Language.Assembly.X86.Instruction where

-- | All opcodes are represented by this enumeration type.

import Data.Word

data Opcode = InvalidOpcode
            | AAA
            | AAD
            | AAM
            | AAS
            | ADC
            | ADD
            | ADDPD
            | ADDPS
            | ADDSD
            | ADDSS
            | ADDSUBPD
            | ADDUBPS
            | AND
            | ANDNPD
            | ANDNPS
            | ANDPD
            | ANDPS
            | ARPL
            | BOUND
            | BSF
            | BSR
            | BT
            | BTC
            | BTR
            | BTS
            | CALL
            | CALLF
            | CBW
            | CDQ
            | CDQE
            | CLC
            | CLD
            | CLFLUSH
            | CLI
            | CLTS
            | CMC
            | CMOVA
            | CMOVB
            | CMOVBE
            | CMOVE
            | CMOVG
            | CMOVGE
            | CMOVL
            | CMOVLE
            | CMOVNB
            | CMOVNE
            | CMOVNO
            | CMOVNP
            | CMOVNS
            | CMOVO
            | CMOVP
            | CMOVS
            | CMP
            | CMPS
            | CMPXCHG
            | CMPXCHG16B
            | CMPXCHG8B
            | COMISD
            | COMISS
            | CPUID
            | CWD
            | CWDE
            | DAA
            | DAS
            | DEC
            | DIV
            | DIVPD
            | DIVPS
            | DIVSD
            | DIVSS
            | EMMS
            | ENTER
            | FABS
            | FADD
            | FADDP
            | FBLD
            | FBSTP
            | FCHS
            | FCLEX
            | FCMOVB
            | FCMOVBE
            | FCMOVE
            | FCMOVNB
            | FCMOVNBE
            | FCMOVNE
            | FCMOVNU
            | FCMOVU
            | FCOM
            | FCOMI
            | FCOMIP
            | FCOMP
            | FCOMPP
            | FDIV
            | FDIVP
            | FDIVR
            | FDIVRP
            | FFREE
            | FIADD
            | FICOM
            | FICOMP
            | FIDIV
            | FIDIVR
            | FILD
            | FIMUL
            | FINIT
            | FIST
            | FISTP
            | FISTPP
            | FISTTP
            | FISUB
            | FISUBR
            | FLD
            | FLD1
            | FLDCW
            | FLDENV
            | FLDL2E
            | FLDL2T
            | FLDLG2
            | FLDLN2
            | FLDPI
            | FLDZ
            | FMUL
            | FMULP
            | FNOP
            | FRSTOR
            | FSAVE
            | FST
            | FSTCW
            | FSTENV
            | FSTP
            | FSTSW
            | FSUB
            | FSUBP
            | FSUBR
            | FSUBRP
            | FTST
            | FUCOM
            | FUCOMI
            | FUCOMIP
            | FUCOMP
            | FUCOMPP
            | FXAM
            | FXCH
            | FXRSTOR
            | FXSAVE
            | HADDPD
            | HADDPS
            | HLT
            | HSUBPD
            | HSUBPS
            | IDIV
            | IMUL
            | BSWAP
            | IN
            | INC
            | INS
            | INT
            | INT3
            | INTO
            | INVD
            | INVLPG
            | IRET
            | JA
            | JB
            | JBE
            | JCXZ
            | JE
            | JG
            | JGE
            | JL
            | JLE
            | JMP
            | JMPF
            | JMPN
            | JNB
            | JNE
            | JNO
            | JNP
            | JNS
            | JO
            | JP
            | JS
            | LAHF
            | LAR
            | LDDQU
            | LDMXCSR
            | LDS
            | LEA
            | LEAVE
            | LES
            | LFENCE
            | LFS
            | LGDT
            | LGS
            | LIDT
            | LLDT
            | LMSW
            | LODS
            | LOOP
            | LOOPE
            | LOOPNE
            | LSL
            | LSS
            | LTR
            | MASKMOVQ
            | MAXPD
            | MAXPS
            | MAXSD
            | MAXSS
            | MFENCE
            | MINPD
            | MINPS
            | MINSD
            | MINSS
            | MONITOR
            | MOV
            | MOVAPD
            | MOVAPS
            | MOVDDUP
            | MOVHPD
            | MOVHPS
            | MOVLHPS
            | MOVLPD
            | MOVLPS
            | MOVLSDUP
            | MOVMSKPD
            | MOVMSKPS
            | MOVNTDQ
            | MOVNTPD
            | MOVNTPS
            | MOVNTQ
            | MOVQ
            | MOVS
            | MOVSD
            | MOVSLDUP
            | MOVSS
            | MOVSXB
            | MOVSXD
            | MOVSXW
            | MOVUPD
            | MOVUPS
            | MOVZXB
            | MOVZXW
            | MUL
            | MULPD
            | MULPS
            | MULSD
            | MULSS
            | MWAIT
            | NEG
            | NOP
            | NOT
            | OR
            | ORPD
            | ORPS
            | OUT
            | OUTS
            | PADDB
            | PADDD
            | PADDQ
            | PADDSB
            | PADDSW
            | PADDUSB
            | PADDUSW
            | PADDW
            | PAND
            | PANDN
            | PAUSE
            | PAVGB
            | PAVGW
            | PMADDWD
            | PMAXSW
            | PMAXUB
            | PMINSW
            | PMINUB
            | PMOVMSKB
            | PMULHUW
            | PMULHW
            | PMULLW
            | PMULUDQ
            | POP
            | POPA
            | POPAD
            | POPF
            | POPFD
            | POPFQ
            | POR
            | PREFETCHNTA
            | PREFETCHT0
            | PREFETCHT1
            | PREFETCHT2
            | PSADBW
            | PSLLD
            | PSLLDQ
            | PSLLQ
            | PSLLW
            | PSRAD
            | PSRAW
            | PSRLD
            | PSRLDQ
            | PSRLQ
            | PSRLW
            | PSUBB
            | PSUBD
            | PSUBQ
            | PSUBSB
            | PSUBSQ
            | PSUBUSB
            | PSUBUSW
            | PSUBW
            | PUSH
            | PUSHA
            | PUSHAD
            | PUSHF
            | PUSHFD
            | PUSHFQ
            | PXOR
            | RCL
            | RCPPS
            | RCPSS
            | RCR
            | RDMSR
            | RDPMC
            | RDTSC
            | RET
            | RETF
            | ROL
            | ROR
            | RSM
            | RSQRTPS
            | RSQRTSS
            | SAHF
            | SAR
            | SBB
            | SCAS
            | SETA
            | SETB
            | SETBE
            | SETE
            | SETG
            | SETGE
            | SETL
            | SETLE
            | SETNB
            | SETNE
            | SETNO
            | SETNP
            | SETNS
            | SETO
            | SETP
            | SETS
            | SFENCE
            | SGDT
            | SHL
            | SHLD
            | SHR
            | SHRD
            | SIDT
            | SLDT
            | SMSW
            | SQRTPD
            | SQRTPS
            | SQRTSD
            | SQRTSS
            | STC
            | STD
            | STI
            | STMXCSR
            | STOS
            | STR
            | SUB
            | SUBPD
            | SUBPS
            | SUBSD
            | SUBSS
            | SWAPGS
            | SYSCALL
            | SYSENTER
            | SYSEXIT
            | TEST
            | UCOMISD
            | UCOMISS
            | UD2
            | UNPCKHPD
            | UNPCKHPS
            | UNPCKLPD
            | UNPCKLPS
            | VERR
            | VERW
            | VMCALL
            | VMCLEAR
            | VMLAUNCH
            | VMPTRLD
            | VMPTRST
            | VMREAD
            | VMRESUME
            | VMWRITE
            | VMXOFF
            | VMXON
            | WAIT
            | WBINVD
            | WRMSR
            | XADD
            | XCHG
            | XLAT
            | XOR
            | XORPD
            | XORPS
            deriving (Show, Eq)

-- | All operands are in one of the following locations:
--
-- - Constants in the instruction stream
--
-- - Memory locations
--
-- - Registers
--
-- Memory locations are referred to by on of several addressing modes:
--
-- - Absolute (address in instruction stream)
--
-- - Register-indirect (address in register)
--
-- - Register-indirect with displacement
--
-- - Base-Index with scale
--
-- - Base-Index with scale and displacement
--
-- Displacements can be encoded as 8 or 32-bit immediates in the
-- instruction stream, but are encoded as Int in instructions for
-- simplicity.
--
data Operand
 = OpImm Word32                                           -- ^ Immediate value
 | OpAddr Word32 InstrOperandSize                         -- ^ Absolute address
 | OpReg String Int                                       -- ^ Register
 | OpFPReg Int                                            -- ^ Floating-point register
 | OpInd String InstrOperandSize                          -- ^ Register-indirect
 | OpIndDisp String Int InstrOperandSize                  -- ^ Register-indirect with displacement
 | OpBaseIndex String String Int InstrOperandSize         -- ^ Base plus scaled index
 | OpIndexDisp String Int Int InstrOperandSize            -- ^ Scaled index with displacement
 | OpBaseIndexDisp String String Int Int InstrOperandSize -- ^ Base plus scaled index with displacement
 deriving (Eq)

-- | Encodes the default and currently active operand or address size.  Can
-- be changed with the operand- or address-size prefixes 0x66 and 0x67.

data OperandSize = BIT16 | BIT32

-- | Some opcodes can operate on data of several widths.  This information
-- is encoded in instructions using the following enumeration type..

data InstrOperandSize = OPNONE -- ^ No operand size specified
                      | OP8    -- ^ 8-bit integer operand
                      | OP16   -- ^ 16-bit integer operand
                      | OP32   -- ^ 32-bit integer operand
                      | OP64   -- ^ 64-bit integer operand
                      | OP128  -- ^ 128-bit integer operand
                      | OPF32  -- ^ 32-bit floating point operand
                      | OPF64  -- ^ 64-bit floating point operand
                      | OPF80  -- ^ 80-bit floating point operand
                      deriving (Show, Eq)

-- | The disassembly routines return lists of the following datatype.  It
-- encodes both invalid byte sequences (with a useful error message, if
-- possible), or a valid instruction.  Both variants contain the list of
-- opcode bytes from which the instruction was decoded and the address of
-- the instruction.

data Instruction
  = BadInstruction Word8 String Int [Word8]  -- ^ Invalid instruction
  | PseudoInstruction Int String             -- ^ Pseudo instruction, e.g. label
  | Instruction { opcode :: Opcode             -- ^ Opcode of the instruction
                , opsize :: InstrOperandSize   -- ^ Operand size, if any
                , operands :: [Operand]        -- ^ Instruction operand
                , address :: Int               -- ^ Start address of instruction
                , bytes ::[Word8]              -- ^ Instruction bytes
                }                            -- ^ Valid instruction
  deriving (Eq)


data Instr = Bad Word8 String
            | Instr Opcode InstrOperandSize [Operand]
