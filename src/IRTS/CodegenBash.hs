module IRTS.CodegenBash (codegenBash) where

import Data.Char
import IRTS.CodegenCommon
import IRTS.Lang
import IRTS.Simplified
import Idris.Core.TT

import Paths_idris_bash

codegenBash :: CodeGenerator
codegenBash ci = do
    preludeName <- getDataFileName "prelude.sh"
    prelude <- readFile preludeName
    writeFile (outputFile ci) $
      prelude ++ "\n\n" ++
      concatMap doCodegen (simpleDecls ci) ++
      name (sMN 0 "runMain") ++ "\n"

doCodegen :: (Name, SDecl) -> String
doCodegen (n, SFun _ args _i def) =
    cgFun n args def

name :: Name -> String
name n =
    "idris_" ++ concatMap char (showCG n)
  where
    char x | isAlpha x || isDigit x = [x]
           | otherwise              = "_" ++ show (fromEnum x) ++ "_"

cr :: Int -> String
cr l = "\n" ++ replicate l '\t'

loc :: Int -> String
loc i = "_THSFRM[" ++ show i ++ "]"

ret :: String
ret = "_R"

dRet :: String
dRet = "${" ++ ret ++ "}"

var :: LVar -> String
var (Loc i)  = loc i
var (Glob n) = name n

dVar :: LVar -> String
dVar x = "${" ++ var x ++ "}"

qVar :: LVar -> String
qVar x = "\"" ++ dVar x ++ "\""

cgFun :: Name -> [Name] -> SExp -> String
cgFun n _args def =
    name n ++ " () {" ++ cr 1 ++
    cgBody 1 ret def ++ "\n}\n\n\n"

cgBody :: Int -> String -> SExp -> String
cgBody l r (SV (Glob f))        = "idris_pushFrame" ++ cr l ++
                                  name f ++ cr l ++
                                  "idris_popFrame" ++
                                  cgRet l r
cgBody _ r (SV (Loc i))         = r ++ "=" ++ dVar (Loc i)
cgBody l r (SApp _ f vs)        = "idris_pushFrame " ++ showSep " " (map qVar vs) ++ cr l ++
                                  name f ++ cr l ++
                                  "idris_popFrame" ++
                                  cgRet l r
cgBody l r (SLet (Loc i) e1 e2) = cgBody l (loc i) e1 ++ cr l ++ cgBody l r e2
-- cgBody l r (SUpdate _ e)
-- cgBody l r (SProj v i)
cgBody l r (SCon _ t _ vs)      = "idris_makeArray " ++ showSep " " (show t : map qVar vs) ++
                                  cgRet l r
cgBody l r (SCase _ v cs)       = cgSwitch l r v cs
cgBody l r (SChkCase v cs)      = cgSwitch l r v cs
cgBody _ r (SConst c)           = r ++ "=" ++ cgConst c
cgBody _ r (SOp o vs)           = cgOp r o vs
cgBody _ r SNothing             = r ++ "=0"
-- cgBody l r (SError x)
cgBody _ _ x                    = error $ "Expression " ++ show x ++ " is not supported"

cgRet :: Int -> String -> String
cgRet l r | r == ret  = ""
          | otherwise = cr l ++ r ++ "=" ++ dRet

cgSwitch :: Int -> String -> LVar -> [SAlt] -> String
cgSwitch l r v cs =
    let
      hasConCase = any isConCase cs
    in
      (if hasConCase
        then "idris_indexArray " ++ qVar v ++ " 0" ++ cr l
        else "") ++
      "case " ++ (if hasConCase then dRet else dVar v) ++ " in" ++ cr l ++
      showSep (cr (l + 1) ++ ";;" ++ cr l) (map (cgCase (l + 1) r v) cs) ++ cr l ++
      "esac"
  where
    isConCase (SConCase _ _ _ _ _) = True
    isConCase _                    = False

cgCase :: Int -> String -> LVar -> SAlt -> String
cgCase l r _ (SDefaultCase e)        = "*)" ++ cr l ++
                                       cgBody l r e
cgCase l r _ (SConstCase t e)        = show t ++ ")" ++ cr l ++
                                       cgBody l r e
cgCase l r v (SConCase i0 t _ ns0 e) = show t ++ ")" ++ cr l ++
                                       project 1 i0 ns0 ++
                                       cgBody l r e
  where
    project :: Int -> Int -> [Name] -> String
    project _ _ []       = ""
    project k i (_ : ns) = "idris_indexArray " ++ qVar v ++ " " ++ show k ++ cr l ++
                           loc i ++ "=" ++ dRet ++ cr l ++
                           project (k + 1) (i + 1) ns

cgConst :: Const -> String
cgConst (I i)             = show i
cgConst (BI i)            = show i
-- cgConst (Ch c)
cgConst (Str s)           = "'" ++ s ++ "'"
cgConst TheWorld          = "0"
cgConst x | isTypeConst x = "0"
          | otherwise     = error $ "Constant " ++ show x ++ " is not supported"

cgOp :: String -> PrimFn -> [LVar] -> String
cgOp r (LPlus  (ATInt _)) [n, m] = r ++ "=$(( " ++ var n ++ " + "  ++ var m ++ " ))"
cgOp r (LMinus (ATInt _)) [n, m] = r ++ "=$(( " ++ var n ++ " - "  ++ var m ++ " ))"
cgOp r (LTimes (ATInt _)) [n, m] = r ++ "=$(( " ++ var n ++ " * "  ++ var m ++ " ))"
-- cgOp r LUDiv
-- cgOp r LSDiv
-- cgOp r LURem
-- cgOp r LSRem
-- cgOp r LAnd
-- cgOp r LOr
-- cgOp r LXOr
-- cgOp r LCompl
-- cgOp r LSHL
-- cgOp r LLSHR
-- cgOp r LASHR
cgOp r (LEq    (ATInt _)) [n, m] = r ++ "=$(( " ++ var n ++ " == " ++ var m ++ " ))"
-- cgOp r LLt
-- cgOp r LLe
-- cgOp r LGt
-- cgOp r LGe
cgOp r (LSLt   (ATInt _)) [n, m] = r ++ "=$(( " ++ var n ++ " < "  ++ var m ++ " ))"
cgOp r (LSLe   (ATInt _)) [n, m] = r ++ "=$(( " ++ var n ++ " <= " ++ var m ++ " ))"
cgOp r (LSGt   (ATInt _)) [n, m] = r ++ "=$(( " ++ var n ++ " > "  ++ var m ++ " ))"
cgOp r (LSGe   (ATInt _)) [n, m] = r ++ "=$(( " ++ var n ++ " >= " ++ var m ++ " ))"
cgOp r (LSExt _ _)        [n]    = r ++ "="     ++ dVar n
-- cgOp r LZExt
-- cgOp r LTrunc
cgOp r LStrConcat         [s, t] = r ++ "="    ++ dVar s           ++ dVar t
cgOp r LStrLt             [s, t] = r ++ "=[[ " ++ dVar s ++ " < "  ++ qVar t ++ " ]]"
cgOp r LStrEq             [s, t] = r ++ "=[[ " ++ dVar s ++ " = "  ++ qVar t ++ " ]]"
cgOp r LStrLen            [s]    = r ++ "=${#" ++  var s ++ "}"
-- cgOp r LIntFloat
-- cgOp r LFloatInt
cgOp r (LIntStr _)        [n]    = r ++ "=" ++ dVar n
-- cgOp r LStrInt
-- cgOp r LFloatStr
-- cgOp r LStrFloat
-- cgOp r LChInt
-- cgOp r LIntCh
-- cgOp r LBitCast
-- cgOp r LFExp
-- cgOp r LFLog
-- cgOp r LFSin
-- cgOp r LFCos
-- cgOp r LFTan
-- cgOp r LFASin
-- cgOp r LFACos
-- cgOp r LFATan
-- cgOp r LFSqrt
-- cgOp r LFFloor
-- cgOp r LFCeil
-- cgOp r LFNegate
cgOp r LStrHead           [s]    = r ++ "=" ++ "${" ++ var s ++ ":0:1}"
cgOp r LStrTail           [s]    = r ++ "=" ++ "${" ++ var s ++ ":1}"
cgOp r LStrCons           [c, s] = r ++ "=" ++ dVar c ++ dVar s
cgOp r LStrIndex          [s, n] = r ++ "=" ++ "${" ++ var s ++ ":" ++ dVar n ++ ":1}"
-- cgOp r LStrRev
cgOp r LReadStr           [_]    = "idris_readStr \"${" ++ r ++ "}\""
cgOp r LWriteStr          [_, s] = "idris_writeStr " ++ r ++ " " ++ qVar s
-- cgOp r LSystemInfo
-- cgOp r LFork
-- cgOp r LPar
-- cgOp r LExternal
-- cgOp r LNoOp
-- cgOp _ o _                       = error $ "Operator " ++ show o ++ " is not supported"
cgOp _ o _                       = "idris_error 'Operator " ++ show o ++ " is not supported'"
