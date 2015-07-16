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
    "idris_" ++ concatMap bashChar (showCG n)
  where
    bashChar x | isAlpha x || isDigit x = [x]
               | otherwise              = "_" ++ show (fromEnum x) ++ "_"

cr :: Int -> String
cr l = "\n" ++ replicate l '\t'

ret :: String
ret = "_R"

dRet :: String
dRet = "${" ++ ret ++ "}"

loc :: Int -> String
loc i = "_THSFRM[" ++ show i ++ "]"

var :: LVar -> String
var (Loc i)  = loc i
var (Glob n) = name n

dVar :: LVar -> String
dVar x = "${" ++ var x ++ "}"

qVar :: LVar -> String
qVar x = "\"" ++ dVar x ++ "\""

cgConst :: Const -> String
cgConst (I i)             = show i
cgConst (BI i)            = show i
cgConst (Ch c)            = "'" ++ [c] ++ "'"
cgConst (Str s)           = "'" ++ s ++ "'"
cgConst TheWorld          = "0"
cgConst x | isTypeConst x = "0"
          | otherwise     = error $ "Constant " ++ show x ++ " is not supported"

cgFun :: Name -> [Name] -> SExp -> String
cgFun n _args def =
    name n ++ " () {" ++ cr 1 ++
    cgFunBody 1 ret def ++ "\n}\n\n\n"

cgFunBody :: Int -> String -> SExp -> String
cgFunBody l r (SV (Glob fun))      = "idris_pushFrame" ++ cr l ++
                                     name fun ++ cr l ++
                                     "idris_popFrame" ++
                                     retCall l r
cgFunBody _ r (SV (Loc i))         = r ++ "=" ++ dVar (Loc i)
cgFunBody l r (SApp _ fun vars)    = "idris_pushFrame " ++ showSep " " (map qVar vars) ++ cr l ++
                                     name fun ++ cr l ++
                                     "idris_popFrame" ++
                                     retCall l r
cgFunBody l r (SLet (Loc i) e1 e2) = cgFunBody l (loc i) e1 ++ cr l ++ cgFunBody l r e2
-- cgFunBody l r (SUpdate _ e)
-- cgFunBody l r (SProj v i)
cgFunBody l r (SCon _ t _ vars)    = "idris_makeArray " ++ showSep " " (show t : map qVar vars) ++
                                     retCall l r
cgFunBody l r (SCase _ v alts)     = cgCase l r v alts
cgFunBody l r (SChkCase v alts)    = cgCase l r v alts
cgFunBody _ r (SConst c)           = r ++ "=" ++ cgConst c
cgFunBody _ r (SOp op args)        = cgOp r op args
cgFunBody _ r SNothing             = r ++ "=0"
-- cgFunBody l r (SError x)
cgFunBody _ _ x                    = error $ "Expression " ++ show x ++ " is not supported"

retCall :: Int -> String -> String
retCall l r = if r == ret then "" else cr l ++ r ++ "=" ++ dRet

conCase :: SAlt -> Bool
conCase (SConCase _ _ _ _ _) = True
conCase _                    = False

cgCase :: Int -> String -> LVar -> [SAlt] -> String
cgCase l r v alts = (if hasConCase
                         then "idris_indexArray " ++ qVar v ++ " 0" ++ cr l
                         else "") ++
                       "case " ++ (if hasConCase then dRet else dVar v) ++ " in" ++ cr l ++
                       showSep (cr (l + 1) ++ ";;" ++ cr l) (map (cgAlt l r v) alts) ++ cr l ++
                       "esac"
  where
    hasConCase :: Bool
    hasConCase = any conCase alts

cgAlt :: Int -> String -> LVar -> SAlt -> String
cgAlt l r _ (SDefaultCase e)        = "*)" ++ cr (l + 1) ++ cgFunBody (l + 1) r e
cgAlt l r _ (SConstCase t e)        = show t ++ ")" ++ cr (l + 1) ++ cgFunBody (l + 1) r e
cgAlt l r v (SConCase i0 t _ ns0 e) = show t ++ ")" ++ cr (l + 1) ++ project 1 i0 ns0 ++ cgFunBody (l + 1) r e
  where
    project :: Int -> Int -> [Name] -> String
    project _ _ []       = ""
    project k i (_ : ns) = "idris_indexArray " ++ qVar v ++ " " ++ show k ++ cr (l + 1) ++
                           loc i ++ "=" ++ dRet ++ cr (l + 1) ++
                           project (k + 1) (i + 1) ns

cgOp :: String -> PrimFn -> [LVar] -> String
cgOp r (LPlus  (ATInt _)) [x, y] = r ++ "=$(( " ++ var x ++ " + "  ++ var y ++ " ))"
cgOp r (LMinus (ATInt _)) [x, y] = r ++ "=$(( " ++ var x ++ " - "  ++ var y ++ " ))"
cgOp r (LTimes (ATInt _)) [x, y] = r ++ "=$(( " ++ var x ++ " * "  ++ var y ++ " ))"
-- cgOp r LUDiv _
-- cgOp r LSDiv _
-- cgOp r LURem _
-- cgOp r LSRem _
-- cgOp r LAnd _
-- cgOp r LOr _
-- cgOp r LXOr _
-- cgOp r LCompl _
-- cgOp r LSHL _
-- cgOp r LLSHR _
-- cgOp r LASHR _
cgOp r (LEq    (ATInt _)) [x, y] = r ++ "=$(( " ++ var x ++ " == " ++ var y ++ " ))"
-- cgOp r LLt _
-- cgOp r LLe _
-- cgOp r LGt _
-- cgOp r LGe _
cgOp r (LSLt   (ATInt _)) [x, y] = r ++ "=$(( " ++ var x ++ " < "  ++ var y ++ " ))"
cgOp r (LSLe   (ATInt _)) [x, y] = r ++ "=$(( " ++ var x ++ " <= " ++ var y ++ " ))"
cgOp r (LSGt   (ATInt _)) [x, y] = r ++ "=$(( " ++ var x ++ " > "  ++ var y ++ " ))"
cgOp r (LSGe   (ATInt _)) [x, y] = r ++ "=$(( " ++ var x ++ " >= " ++ var y ++ " ))"
cgOp r (LSExt _ _)        [x]    = r ++ "=" ++ dVar x
-- cgOp r LZExt _ _
cgOp r (LTrunc _ _)       [x]    = r ++ "=" ++ dVar x
cgOp r LStrConcat         [x, y] = r ++ "=" ++ dVar x ++ dVar y
cgOp r LStrLt             [x, y] = r ++ "=[[ " ++ dVar x ++ " < " ++ qVar y ++ " ]]"
cgOp r LStrEq             [x, y] = r ++ "=[[ " ++ dVar x ++ " = " ++ qVar y ++ " ]]"
cgOp r LStrLen            [x]    = r ++ "=${#" ++ var x ++ "}"
-- cgOp r LIntFloat _
-- cgOp r LFloatInt _
cgOp r (LIntStr _)        [x]    = r ++ "=" ++ dVar x
-- cgOp r LStrInt _
-- cgOp r LFloatStr
-- cgOp r LStrFloat
cgOp r (LChInt _)         [x]    = r ++ "=" ++ dVar x
cgOp r (LIntCh _)         [x]    = r ++ "=" ++ dVar x
-- cgOp r LBitCast _ _
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
cgOp r LStrHead           [x]    = r ++ "=" ++ "${" ++ var x ++ ":0:1}"
cgOp r LStrTail           [x]    = r ++ "=" ++ "${" ++ var x ++ ":1}"
cgOp r LStrCons           [x, y] = r ++ "=" ++ dVar x ++ dVar y
cgOp r LStrIndex          [x, y] = r ++ "=" ++ "${" ++ var x ++ ":" ++ dVar y ++ ":1}"
-- cgOp r LStrRev
cgOp r LReadStr           [_]    = "idris_readStr \"${" ++ r ++ "}\""
cgOp r LWriteStr          [_, x] = "idris_writeStr " ++ r ++ " " ++ qVar x
-- cgOp r LSystemInfo
-- cgOp r LFork
-- cgOp r LPar
-- cgOp r LExternal _
-- cgOp r LNoOp
-- cgOp _ op _                      = error $ "Operator " ++ show op ++ " is not supported"
cgOp _ op _                      = "idris_error 'Operator " ++ show op ++ " is not supported'"
