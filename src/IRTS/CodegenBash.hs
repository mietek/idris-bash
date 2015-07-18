module IRTS.CodegenBash (codegenBash) where

import Data.Char
import IRTS.CodegenCommon
import IRTS.CodegenUtils
import IRTS.Lang
import IRTS.Simplified
import Idris.Core.TT

import Paths_idris_bash


codegenBash :: CodeGenerator
codegenBash ci = do
    preludeName <- getDataFileName "prelude.sh"
    prelude <- readFile preludeName
    let tm = findTags ci
    writeFile (outputFile ci) $
      prelude ++ "\n\n" ++
      cgTags tm ++ "\n\n" ++
      concatMap (cgFun tm) (simpleDecls ci) ++
      name (sMN 0 "runMain") ++ "\n"


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


cgLoc :: Int -> String
cgLoc i = loc i


cgVar :: LVar -> String
cgVar (Loc i) = loc i
cgVar x       = error $ "Variable " ++ show x ++ " is not supported"


cgPVar :: LVar -> String
cgPVar x = "${" ++ cgVar x ++ "}"


cgQPVar :: LVar -> String
cgQPVar x = "\"" ++ cgPVar x ++ "\""


cgTags :: TagMap -> String
cgTags tm = showSep "\n" (map tag (askTags tm)) ++ "\n" ++
            "_AP=" ++ show (askTagCount tm) ++ "\n"
  where
    tag (t, ap) = "_A[" ++ show ap ++ "]=" ++ show t


cgFun :: TagMap -> (Name, SDecl) -> String
cgFun tm (n, f@(SFun _ args _ e)) =
    name n ++ " () {" ++ cr 1 ++
    pushFrame ++
    moveArgs ++
    sizeFrame ++
    cgBody tm 1 "_R" e ++
    popFrame ++ "\n}\n\n\n"
  where
    argCount  = length args
    locCount  = countLocs f
    frameSize = max argCount locCount
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


cgBody :: TagMap -> Int -> String -> SExp -> String
cgBody _  l r (SV (Glob f))        = name f ++
                                     cgRet l r
cgBody _  _ r (SV v@(Loc _))       = r ++ "=" ++ cgPVar v
cgBody _  l r (SApp _ f vs)        = name f ++ " " ++ showSep " " (map cgQPVar vs) ++
                                     cgRet l r
cgBody tm l r (SLet (Loc i) e1 e2) = cgBody tm l (cgLoc i) e1 ++ cr l ++
                                     cgBody tm l r e2
-- cgBody tm l r (SUpdate _ e)
-- cgBody tm l r (SProj v i)
cgBody tm _ r (SCon _ t _ [])      = r ++ "=" ++ show (askTag tm t)
cgBody _  l r (SCon _ t _ vs)      = cgArray l r (show t : map cgPVar vs)
cgBody tm l r (SCase _ v cs)       = cgSwitch tm l r v cs
cgBody tm l r (SChkCase v cs)      = cgSwitch tm l r v cs
cgBody _  _ r (SConst c)           = r ++ "=" ++ cgConst c
cgBody _  _ r (SOp o vs)           = cgOp r o vs
cgBody _  _ r SNothing             = r ++ "=0"
-- cgBody tm l r (SError x)
cgBody _  _ _ x                    = error $ "Expression " ++ show x ++ " is not supported"


cgRet :: Int -> String -> String
cgRet l r | r == "_R" = ""
          | otherwise = cr l ++ r ++ "=${_R}"


cgArray :: Int -> String -> [String] -> String
cgArray l r args =
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


cgSwitch :: TagMap -> Int -> String -> LVar -> [SAlt] -> String
cgSwitch tm l r v cs =
    let
      v' = if any isConCase cs then "${_A[" ++ cgVar v ++ "]}" else cgPVar v
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
    project k i (_ : ns) = cgLoc i ++ "=${_A[" ++ cgVar v ++ " + " ++ show k ++ "]}" ++ cr l ++
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
cgOp r (LPlus  (ATInt _)) [n, m] = r ++ "=$(( " ++ cgVar n ++ " + "  ++ cgVar m ++ " ))"
cgOp r (LMinus (ATInt _)) [n, m] = r ++ "=$(( " ++ cgVar n ++ " - "  ++ cgVar m ++ " ))"
cgOp r (LTimes (ATInt _)) [n, m] = r ++ "=$(( " ++ cgVar n ++ " * "  ++ cgVar m ++ " ))"
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
cgOp r (LEq    (ATInt _)) [n, m] = r ++ "=$(( " ++ cgVar n ++ " == " ++ cgVar m ++ " ))"
-- cgOp r LLt
-- cgOp r LLe
-- cgOp r LGt
-- cgOp r LGe
cgOp r (LSLt   (ATInt _)) [n, m] = r ++ "=$(( " ++ cgVar n ++ " < "  ++ cgVar m ++ " ))"
cgOp r (LSLe   (ATInt _)) [n, m] = r ++ "=$(( " ++ cgVar n ++ " <= " ++ cgVar m ++ " ))"
cgOp r (LSGt   (ATInt _)) [n, m] = r ++ "=$(( " ++ cgVar n ++ " > "  ++ cgVar m ++ " ))"
cgOp r (LSGe   (ATInt _)) [n, m] = r ++ "=$(( " ++ cgVar n ++ " >= " ++ cgVar m ++ " ))"
cgOp r (LSExt _ _)        [n]    = r ++ "=" ++ cgPVar n
-- cgOp r LZExt
-- cgOp r LTrunc
cgOp r LStrConcat         [s, t] = r ++ "=" ++ cgPVar s ++ cgPVar t
-- cgOp r LStrLt
-- cgOp r LStrEq
cgOp r LStrLen            [s]    = r ++ "=${#" ++  cgVar s ++ "}"
-- cgOp r LIntFloat
-- cgOp r LFloatInt
cgOp r (LIntStr _)        [n]    = r ++ "=" ++ cgPVar n
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
cgOp r LStrHead           [s]    = r ++ "=${" ++ cgVar s ++ ":0:1}"
cgOp r LStrTail           [s]    = r ++ "=${" ++ cgVar s ++ ":1}"
cgOp r LStrCons           [c, s] = r ++ "=" ++ cgPVar c ++ cgPVar s
cgOp r LStrIndex          [s, n] = r ++ "=${" ++ cgVar s ++ ":" ++ cgPVar n ++ ":1}"
-- cgOp r LStrRev
-- cgOp r LReadStr
cgOp _ LWriteStr          [_, s] = "echo " ++ cgQPVar s
-- cgOp r LSystemInfo
-- cgOp r LFork
-- cgOp r LPar
-- cgOp r LExternal
-- cgOp r LNoOp
-- cgOp _ o _                       = error $ "Operator " ++ show o ++ " is not supported"
cgOp _ o _                       = "echo 'Operator " ++ show o ++ " is not supported' >&2"
