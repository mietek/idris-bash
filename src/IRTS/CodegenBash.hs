module IRTS.CodegenBash (codegenBash) where

import Data.Char
import IRTS.CodegenCommon
import IRTS.Lang
import IRTS.Simplified
import Idris.Core.TT

lit :: [String] -> String
lit ls = showSep (cr 1) ls

bash_writeStr :: String
bash_writeStr = lit
    [ "idris_writeStr () {"
    ,   "echo \"$2\""
    ,   "eval \"$1=\""
    ]

bash_readStr :: String
bash_readStr = lit
    [ "idris_readStr () {"
    ,   "read -r \"$1\""
    ]

bash_error :: String
bash_error = lit
    [ "idris_error () {"
    ,   "echo \"$@\" >&2"
    ,   "exit 1"
    ]

bash_switch :: String
bash_switch = lit
    [ "idris_switch () {"
    ,   "local arr"
    ,   "arr=\"${!2}[$3]\""
    -- ,   "echo -e \"${FUNCNAME[1]}\\n\\t${!arr} <- ${arr}\""
    ,   "eval \"$1=\\${!arr}\""
    ]

bash_array :: String
bash_array = lit
    [ "idris_array () {"
    ,   "local arr"
    ,   "arr=${" ++ arrNext ++ "}"
    ,   arrCtr ++ "=$(( " ++ arrCtr ++ " + 1 ))"
    ,   arrNext ++ "=" ++ arr ++ "${" ++ arrCtr ++ "}"
    -- ,   "echo -e \"${FUNCNAME[1]}\\n\\t${arr}=( ${*:2} )\""
    ,   "echo -e \"${arr}=( ${*:2} )\""
    ,   "eval \"${arr}=( \"\\${@:2}\" )\""
    ,   "eval \"$1=\\${arr}\""
    ]

lits :: String
lits = showSep "\n}\n\n\n"
    [ bash_writeStr
    , bash_readStr
    , bash_error
    , bash_switch
    , bash_array
    ]

codegenBash :: CodeGenerator
codegenBash ci = do
    writeFile (outputFile ci) $
      "#!/usr/bin/env bash\n\n\n" ++
      "set -eu\n\n\n" ++
      arrCtr ++ "=0\n" ++
      arrNext ++ "=" ++ arr ++ "0\n\n\n" ++
      lits ++ "\n}\n\n\n" ++
      concatMap doCodegen (simpleDecls ci) ++
      name (sMN 0 "runMain") ++ "\n"

doCodegen :: (Name, SDecl) -> String
doCodegen (n, SFun _ args _i def) =
    cgFun n args def

name :: Name -> String
name n =
    "idris_" ++ concatMap bashChar (showCG n)
  where
    bashChar x | isAlpha x || isDigit x = [x]
               | otherwise              = "_" ++ show (fromEnum x) ++ "_"

cr :: Int -> String
cr l = "\n" ++ replicate l '\t'

arr :: String
arr = "_ARR"

arrCtr :: String
arrCtr = arr ++ "_CTR"

arrNext :: String
arrNext = arr ++ "_NEXT"

ret :: String
ret = "_RET"

dRet :: String
dRet = "${" ++ ret ++ "}"

loc :: String -> Int -> String
loc fn i = fn ++ "_loc" ++ show i ++ "[${#FUNCNAME[@]}]"

var :: String -> LVar -> String
var fn (Loc i)  = loc fn i
var _  (Glob n) = name n

dVar :: String -> LVar -> String
dVar fn x = "${" ++ var fn x ++ "}"

qVar :: String -> LVar -> String
qVar fn x = "\"" ++ dVar fn x ++ "\""

csv :: String
csv = "_CASE"

dCsv :: String
dCsv = "${" ++ csv ++ "}"

cgConst :: Const -> String
cgConst (I i)             = show i
cgConst (BI i)            = show i
cgConst (Ch c)            = "'" ++ [c] ++ "'"
cgConst (Str s)           = "'" ++ s ++ "'"
cgConst TheWorld          = "0"
cgConst x | isTypeConst x = "0"
          | otherwise     = error $ "Constant " ++ show x ++ " is not supported by the bash backend"

cgFun :: Name -> [Name] -> SExp -> String
cgFun n args def =
    fn ++ " () {" ++ cr 1 ++
    cgFunArgs fn args ++ cr 1 ++
    cgFunBody fn 1 ret def ++ "\n}\n\n\n"
  where
    fn :: String
    fn = name n

cgFunArgs :: String -> [Name] -> String
cgFunArgs _  []   = ""
cgFunArgs fn args = showSep (cr 1) (map bind nums) ++ cr 1
  where
    nums   = [0..length args - 1]
    bind i = loc fn i ++ "=$" ++ show (i + 1)

cgFunBody :: String -> Int -> String -> SExp -> String
cgFunBody _  l r (SV (Glob fun))      = name fun ++ retCall l r
cgFunBody fn _ r (SV (Loc i))         = r ++ "=" ++ dVar fn (Loc i)
cgFunBody fn l r (SApp _ fun vars)    = name fun ++ " " ++ showSep " " (map (qVar fn) vars) ++ retCall l r
cgFunBody fn l r (SLet (Loc i) e1 e2) = cgFunBody fn l (loc fn i) e1 ++ cr l ++ cgFunBody fn l r e2
-- cgFunBody fn l r (SUpdate _ e)
-- cgFunBody fn l r (SProj v i)
cgFunBody fn _ r (SCon _ t _ vars)    = "idris_array " ++ r ++ " " ++ showSep " " (show t : map (qVar fn) vars)
cgFunBody fn l r (SCase _ v alts)     = cgCase fn l r v alts
cgFunBody fn l r (SChkCase v alts)    = cgCase fn l r v alts
cgFunBody _  _ r (SConst c)           = r ++ "=" ++ cgConst c
cgFunBody fn _ r (SOp op args)        = cgOp fn r op args
cgFunBody _  _ r SNothing             = r ++ "=0"
-- cgFunBody fn l r (SError x)
cgFunBody _  _ _ x                    = error $ "Expression " ++ show x ++ " is not supported by the bash backend"

retCall :: Int -> String -> String
retCall l r = if r == ret then "" else cr l ++ r ++ "=" ++ dRet

conCase :: SAlt -> Bool
conCase (SConCase _ _ _ _ _) = True
conCase _                    = False

cgCase :: String -> Int -> String -> LVar -> [SAlt] -> String
cgCase fn l r v alts = (if hasConCase
                         then "idris_switch " ++ csv ++ " " ++ var fn v ++ " 0" ++ cr l
                         else "") ++
                       "case " ++ (if hasConCase then dCsv else dVar fn v) ++ " in" ++ cr l ++
                       showSep (cr (l + 1) ++ ";;" ++ cr l) (map (cgAlt fn l r v) alts) ++ cr l ++
                       "esac"
  where
    hasConCase :: Bool
    hasConCase = any conCase alts

cgAlt :: String -> Int -> String -> LVar -> SAlt -> String
cgAlt fn l r _ (SDefaultCase e)        = "*)" ++ cr (l + 1) ++ cgFunBody fn (l + 1) r e
cgAlt fn l r _ (SConstCase t e)        = show t ++ ")" ++ cr (l + 1) ++ cgFunBody fn (l + 1) r e
cgAlt fn l r v (SConCase i0 t _ ns0 e) = show t ++ ")" ++ cr (l + 1) ++ project 1 i0 ns0 ++ cgFunBody fn (l + 1) r e
  where
    project :: Int -> Int -> [Name] -> String
    project _ _ []       = ""
    project k i (_ : ns) = "idris_switch " ++ loc fn i ++ " " ++ var fn v ++ " " ++ show k ++ cr (l + 1) ++
                           project (k + 1) (i + 1) ns

cgOp :: String -> String -> PrimFn -> [LVar] -> String
cgOp fn r (LPlus  (ATInt _)) [x, y] = r ++ "=$(( " ++ var fn x ++ " + "  ++ var fn y ++ " ))"
cgOp fn r (LMinus (ATInt _)) [x, y] = r ++ "=$(( " ++ var fn x ++ " - "  ++ var fn y ++ " ))"
cgOp fn r (LTimes (ATInt _)) [x, y] = r ++ "=$(( " ++ var fn x ++ " * "  ++ var fn y ++ " ))"
-- cgOp fn r LUDiv _
-- cgOp fn r LSDiv _
-- cgOp fn r LURem _
-- cgOp fn r LSRem _
-- cgOp fn r LAnd _
-- cgOp fn r LOr _
-- cgOp fn r LXOr _
-- cgOp fn r LCompl _
-- cgOp fn r LSHL _
-- cgOp fn r LLSHR _
-- cgOp fn r LASHR _
cgOp fn r (LEq    (ATInt _)) [x, y] = r ++ "=$(( " ++ var fn x ++ " == " ++ var fn y ++ " ))"
-- cgOp fn r LLt _
-- cgOp fn r LLe _
-- cgOp fn r LGt _
-- cgOp fn r LGe _
cgOp fn r (LSLt   (ATInt _)) [x, y] = r ++ "=$(( " ++ var fn x ++ " < "  ++ var fn y ++ " ))"
cgOp fn r (LSLe   (ATInt _)) [x, y] = r ++ "=$(( " ++ var fn x ++ " <= " ++ var fn y ++ " ))"
cgOp fn r (LSGt   (ATInt _)) [x, y] = r ++ "=$(( " ++ var fn x ++ " > "  ++ var fn y ++ " ))"
cgOp fn r (LSGe   (ATInt _)) [x, y] = r ++ "=$(( " ++ var fn x ++ " >= " ++ var fn y ++ " ))"
cgOp fn r (LSExt _ _)        [x]    = r ++ "=" ++ dVar fn x
-- cgOp fn r LZExt _ _
cgOp fn r (LTrunc _ _)       [x]    = r ++ "=" ++ dVar fn x
cgOp fn r LStrConcat         [x, y] = r ++ "=" ++ dVar fn x ++ dVar fn y
cgOp fn r LStrLt             [x, y] = r ++ "=[[ " ++ dVar fn x ++ " < " ++ qVar fn y ++ " ]]"
cgOp fn r LStrEq             [x, y] = r ++ "=[[ " ++ dVar fn x ++ " = " ++ qVar fn y ++ " ]]"
cgOp fn r LStrLen            [x]    = r ++ "=${#" ++ var fn x ++ "}"
-- cgOp fn r LIntFloat _
-- cgOp fn r LFloatInt _
cgOp fn r (LIntStr _)        [x]    = r ++ "=" ++ dVar fn x
-- cgOp fn r LStrInt _
-- cgOp fn r LFloatStr
-- cgOp fn r LStrFloat
cgOp fn r (LChInt _)         [x]    = r ++ "=" ++ dVar fn x
cgOp fn r (LIntCh _)         [x]    = r ++ "=" ++ dVar fn x
-- cgOp fn r LBitCast _ _
-- cgOp fn r LFExp
-- cgOp fn r LFLog
-- cgOp fn r LFSin
-- cgOp fn r LFCos
-- cgOp fn r LFTan
-- cgOp fn r LFASin
-- cgOp fn r LFACos
-- cgOp fn r LFATan
-- cgOp fn r LFSqrt
-- cgOp fn r LFFloor
-- cgOp fn r LFCeil
-- cgOp fn r LFNegate
cgOp fn r LStrHead           [x]    = r ++ "=" ++ "${" ++ var fn x ++ ":0:1}"
cgOp fn r LStrTail           [x]    = r ++ "=" ++ "${" ++ var fn x ++ ":1}"
cgOp fn r LStrCons           [x, y] = r ++ "=" ++ dVar fn x ++ dVar fn y
cgOp fn r LStrIndex          [x, y] = r ++ "=" ++ "${" ++ var fn x ++ ":" ++ dVar fn y ++ ":1}"
-- cgOp fn r LStrRev
cgOp _  r LReadStr           [_]    = "idris_readStr \"${" ++ r ++ "}\""
cgOp fn r LWriteStr          [_, x] = "idris_writeStr " ++ r ++ " " ++ qVar fn x
-- cgOp fn r LSystemInfo
-- cgOp fn r LFork
-- cgOp fn r LPar
-- cgOp fn r LExternal _
-- cgOp fn r LNoOp
-- cgOp _  _ op _                      = error $ "Operator " ++ show op ++ " is not supported by the bash backend"
cgOp _  _ op _                      = "idris_error 'Operator " ++ show op ++ " is not supported by the bash backend'"
