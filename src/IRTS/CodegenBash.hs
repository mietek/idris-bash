module IRTS.CodegenBash (codegenBash) where

import Data.Char
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M
import Data.List
import IRTS.CodegenCommon
import IRTS.Lang
import IRTS.Simplified
import Idris.Core.TT

import Paths_idris_bash

type TagMap = IntMap Int

codegenBash :: CodeGenerator
codegenBash ci = do
    preludeName <- getDataFileName "prelude.sh"
    prelude <- readFile preludeName
    let tm = findTags ci
    writeFile (outputFile ci) $
      prelude ++ "\n\n" ++
      cgTags tm ++ "\n\n" ++
      concatMap (doCodegen tm) (simpleDecls ci) ++
      name (sMN 0 "runMain") ++ "\n"

findTags :: CodegenInfo -> TagMap
findTags ci = M.fromAscList (zip ts [0..])
  where
    ts = nub (sort (concatMap ftFun (simpleDecls ci)))

ftFun :: (Name, SDecl) -> [Int]
ftFun (_, SFun _ _ _ e) = ftExp e

ftExp :: SExp -> [Int]
ftExp (SLet (Loc _) e1 e2) = ftExp e1 ++ ftExp e2
ftExp (SCase _ _ cs)       = concatMap ftCase cs
ftExp (SChkCase _ cs)      = concatMap ftCase cs
ftExp (SCon _ t _ [])      = [t]
ftExp _                    = []

ftCase :: SAlt -> [Int]
ftCase (SDefaultCase e)     = ftExp e
ftCase (SConstCase _ e)     = ftExp e
ftCase (SConCase _ _ _ _ e) = ftExp e

cgTags :: TagMap -> String
cgTags tm = showSep "\n" (map tag (M.toAscList tm)) ++ "\n" ++
            "_AP=" ++ show (M.size tm) ++ "\n"
  where
    tag (t, ap) = "_A[" ++ show ap ++ "]=" ++ show t

doCodegen :: TagMap -> (Name, SDecl) -> String
doCodegen tm (n, SFun _ args _ e) =
    cgFun tm n (length args) e

name :: Name -> String
name n =
    "idris_" ++ concatMap char (showCG n)
  where
    char x | isAlpha x || isDigit x = [x]
           | otherwise              = "_" ++ show (fromEnum x) ++ "_"

cr :: Int -> String
cr l = "\n" ++ replicate l '\t'

loc :: Int -> String
loc 0 = "_S[_SP]"
loc i = "_S[_SP + " ++ show i ++ "]"

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

cgFun :: TagMap -> Name -> Int -> SExp -> String
cgFun tm n argCount e =
    name n ++ " () {" ++ cr 1 ++
    pushFrame ++
    moveArgs ++
    sizeFrame ++
    cgBody tm 1 ret e ++
    popFrame ++ "\n}\n\n\n"
  where
    frameSize = max argCount (locCountBody e)
    pushFrame | frameSize == 0 = ""
              | otherwise      = "_PSP[_SR]=${_SP}; _SP=${_SQ}; _SR=$(( _SR + 1 ))" ++ cr 1
    moveArgs  | argCount == 0  = ""
              | otherwise      = showSep (cr 1) (map moveArg [1..argCount]) ++ cr 1
    moveArg 1 = "_S[_SP]=$1"
    moveArg i = "_S[_SP + " ++ show (i - 1) ++ "]=$" ++ show i
    sizeFrame | frameSize == 0 = ""
              | otherwise      = "_SQ=$(( _SP + " ++ show frameSize ++ " ))" ++ cr 1
    popFrame  | frameSize == 0 = ""
              | otherwise      = cr 1 ++ "_SQ=${_SP}; _SR=$(( _SR - 1 )); _SP=${_PSP[_SR]}"

locCountBody :: SExp -> Int
locCountBody (SV (Loc i))         = i + 1
locCountBody (SLet (Loc i) e1 e2) = max (i + 1) (max (locCountBody e1) (locCountBody e2))
locCountBody (SCase _ _ cs)       = maximum (map locCountCase cs)
locCountBody (SChkCase _ cs)      = maximum (map locCountCase cs)
locCountBody _                    = 0

locCountCase :: SAlt -> Int
locCountCase (SDefaultCase e)      = locCountBody e
locCountCase (SConstCase _ e)      = locCountBody e
locCountCase (SConCase _ _ _ [] e) = locCountBody e
locCountCase (SConCase i _ _ ns e) = max (i + length ns) (locCountBody e)

cgBody :: TagMap -> Int -> String -> SExp -> String
cgBody _  l r (SV (Glob f))        = name f ++
                                     cgRet l r
cgBody _  _ r (SV (Loc i))         = r ++ "=" ++ dVar (Loc i)
cgBody _  l r (SApp _ f vs)        = name f ++ " " ++ showSep " " (map qVar vs) ++
                                     cgRet l r
cgBody tm l r (SLet (Loc i) e1 e2) = cgBody tm l (loc i) e1 ++ cr l ++
                                     cgBody tm l r e2
-- cgBody tm l r (SUpdate _ e)
-- cgBody tm l r (SProj v i)
cgBody tm _ r (SCon _ t _ [])      = r ++ "=" ++ show (tm M.! t)
cgBody _  l r (SCon _ t _ vs)      = makeArray l r (show t : map qVar vs)
cgBody tm l r (SCase _ v cs)       = cgSwitch tm l r v cs
cgBody tm l r (SChkCase v cs)      = cgSwitch tm l r v cs
cgBody _  _ r (SConst c)           = r ++ "=" ++ cgConst c
cgBody _  _ r (SOp o vs)           = cgOp r o vs
cgBody _  _ r SNothing             = r ++ "=0"
-- cgBody tm l r (SError x)
cgBody _  _ _ x                    = error $ "Expression " ++ show x ++ " is not supported"

makeArray :: Int -> String -> [String] -> String
makeArray l r args =
    makeElements ++
    r ++ "=${_AP}" ++
    -- cr l ++ "echo \"${_AP}\" " ++ showSep (" ") args ++
    pushArray
  where
    argCount = length args
    makeElements | argCount == 0 = ""
                 | otherwise     = showSep (cr l) (map makeElement (zip [1..argCount] args)) ++ cr l
    makeElement (1, arg) = "_A[_AP]=" ++ arg
    makeElement (i, arg) = "_A[_AP + " ++ show (i - 1) ++ "]=" ++ arg
    pushArray    | argCount == 0 = ""
                 | otherwise     = cr l ++ "_AP=$(( _AP + " ++ show argCount ++ " ))"

cgRet :: Int -> String -> String
cgRet l r | r == ret  = ""
          | otherwise = cr l ++ r ++ "=" ++ dRet

cgSwitch :: TagMap -> Int -> String -> LVar -> [SAlt] -> String
cgSwitch tm l r v cs =
    let
      v' = if any isConCase cs then "${_A[" ++ var v ++ "]}" else dVar v
    in
      "case " ++ v' ++ " in" ++ cr l ++
      showSep (cr (l + 1) ++ ";;" ++ cr l) (map (cgCase tm (l + 1) r v) cs) ++ cr l ++
      "esac"
  where
    isConCase (SConCase _ _ _ _ _) = True
    isConCase _                    = False

cgCase :: TagMap -> Int -> String -> LVar -> SAlt -> String
cgCase tm l r _ (SDefaultCase e)        = "*)" ++ cr l ++
                                          cgBody tm l r e
cgCase tm l r _ (SConstCase t e)        = show t ++ ")" ++ cr l ++
                                          cgBody tm l r e
cgCase tm l r v (SConCase i0 t _ ns0 e) = show t ++ ")" ++ cr l ++
                                          project 1 i0 ns0 ++
                                          cgBody tm l r e
  where
    project :: Int -> Int -> [Name] -> String
    project _ _ []       = ""
    project k i (_ : ns) = loc i ++ "=${_A[" ++ var v ++ " + " ++ show k ++ "]}" ++ cr l ++
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
cgOp r (LSExt _ _)        [n]    = r ++ "=" ++ dVar n
-- cgOp r LZExt
-- cgOp r LTrunc
cgOp r LStrConcat         [s, t] = r ++ "=" ++ dVar s ++ dVar t
-- cgOp r LStrLt
-- cgOp r LStrEq
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
cgOp r LStrHead           [s]    = r ++ "=${" ++ var s ++ ":0:1}"
cgOp r LStrTail           [s]    = r ++ "=${" ++ var s ++ ":1}"
cgOp r LStrCons           [c, s] = r ++ "=" ++ dVar c ++ dVar s
cgOp r LStrIndex          [s, n] = r ++ "=${" ++ var s ++ ":" ++ dVar n ++ ":1}"
-- cgOp r LStrRev
-- cgOp r LReadStr
cgOp _ LWriteStr          [_, s] = "echo " ++ qVar s
-- cgOp r LSystemInfo
-- cgOp r LFork
-- cgOp r LPar
-- cgOp r LExternal
-- cgOp r LNoOp
-- cgOp _ o _                       = error $ "Operator " ++ show o ++ " is not supported"
cgOp _ o _                       = "echo 'Operator " ++ show o ++ " is not supported' >&2"
