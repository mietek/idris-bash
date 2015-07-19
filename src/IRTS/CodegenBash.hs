module IRTS.CodegenBash (codegenBash) where

import Data.Char
import Data.List
import IRTS.CodegenCommon
import IRTS.Codegen.Emitter
import IRTS.Codegen.Utils
import IRTS.Lang
import IRTS.Simplified
import Idris.Core.TT

import Paths_idris_bash


codegenBash :: CodeGenerator
codegenBash ci = do
    preludePath <- getDataFileName "prelude.sh"
    prelude <- readFile preludePath
    let tm = findTags ci
        fs = simpleDecls ci
        outputPath = outputFile ci
        output = collect $ emitProgram prelude tm fs
    writeFile outputPath output


emitProgram :: String -> TagMap -> [(Name, SDecl)] -> Emitter
emitProgram prelude tm fs = do
    emit prelude
    emit ""
    emitTagDefs tm
    mapM_ (emitFunDef tm) fs
    emit $ showName (sMN 0 "runMain")


emitTagDefs :: TagMap -> Emitter
emitTagDefs tm = do
    mapM_ emitTagDef (askTags tm)
    emit $ "_AP=" ++ show (askTagCount tm)
    emit ""
    emit ""

emitTagDef :: (Int, Int) -> Emitter
emitTagDef (t, i) =
    emit $ "_A[" ++ show i ++ "]=" ++ show t


emitFunDef :: TagMap -> (Name, SDecl) -> Emitter
emitFunDef tm (n, f@(SFun _ args _ e)) = do
    emit $ showName n ++ " () {"
    nest $ do
      emitPushFrame fs
      mapM_ emitMoveArg [1..ac]
      emitSizeFrame fs
      emitExp tm "_R" e
      emitPopFrame fs
    emit "}"
    emit ""
    emit ""
  where
    ac = length args
    lc = countLocs f
    fs = max ac lc

emitPushFrame :: Int -> Emitter
emitPushFrame 0 = skip
emitPushFrame _ = emit $ "_PSP[_SR]=${_SP}; _SP=${_SQ}; _SR=$(( _SR + 1 ))"

emitMoveArg :: Int -> Emitter
emitMoveArg 1 = emit $ "_S[_SP]=$1"
emitMoveArg i = emit $ "_S[_SP + " ++ show (i - 1) ++ "]=$" ++ show i

emitSizeFrame :: Int -> Emitter
emitSizeFrame 0  = skip
emitSizeFrame fs = emit $ "_SQ=$(( _SP + " ++ show fs ++ " ))"

emitPopFrame :: Int -> Emitter
emitPopFrame 0 = skip
emitPopFrame _ = emit $ "_SQ=${_SP}; _SR=$(( _SR - 1 )); _SP=${_PSP[_SR]}"


emitExp :: TagMap -> String -> SExp -> Emitter
emitExp _  r (SV (Glob f))        = emitFunCall r f []
emitExp _  r (SV v@(Loc _))       = emit $ r ++ "=" ++ showParamVar v
emitExp _  r (SApp _ f vs)        = emitFunCall r f vs
emitExp tm r (SLet (Loc i) e1 e2) = do
    emitExp tm (showLoc i) e1
    emitExp tm r e2
-- emitExp tm r (SUpdate _ e)
-- emitExp tm r (SProj v i)
emitExp tm r (SCon _ t _ [])      = emit $ r ++ "=" ++ show (askTag tm t)
emitExp _  r (SCon _ t _ vs)      = emitArray r (show t : map showParamVar vs)
emitExp tm r (SCase _ v cs)       = emitSwitch tm r v cs
emitExp tm r (SChkCase v cs)      = emitSwitch tm r v cs
emitExp _  r (SConst c)           = emit $ r ++ "=" ++ showConst c
emitExp _  r (SOp o vs)           = emit $ showOp r o vs
emitExp _  r SNothing             = emit $ r ++ "=0"
-- emitExp tm r (SError x)
emitExp _  _ x                    = error $ "Expression " ++ show x ++ " is not supported"


emitFunCall :: String -> Name -> [LVar] -> Emitter
emitFunCall r f vs = do
    emit $ showSep " " (showName f : map showQuotedParamVar vs)
    if r == "_R"
      then skip
      else emit $ r ++ "=${_R}"


emitArray :: String -> [String] -> Emitter
emitArray r args = do
    mapM_ emitArrayElement (zip [0..] args)
    emit $ r ++ "=${_AP}"
    emitPushArray ac
  where
    ac = length args

emitArrayElement :: (Int, String) -> Emitter
emitArrayElement (0, arg) = emit $ "_A[_AP]=" ++ arg
emitArrayElement (i, arg) = emit $ "_A[_AP + " ++ show i ++ "]=" ++ arg

emitPushArray :: Int -> Emitter
emitPushArray 0  = skip
emitPushArray ac = emit $ "_AP=$(( _AP + " ++ show ac ++ " ))"


emitSwitch :: TagMap -> String -> LVar -> [SAlt] -> Emitter
emitSwitch tm r v cs = do
    let v' = if any isConCase cs then "${_A[" ++ showVar v ++ "]}" else showParamVar v
    emit $ "case " ++ v' ++ " in"
    sequence_ $
      intersperse (nest $ emit ";;") $
        map (emitCase tm r v) cs
    emit "esac"
  where
    isConCase (SConCase _ _ _ _ _) = True
    isConCase _                    = False

emitCase :: TagMap -> String -> LVar -> SAlt -> Emitter
emitCase tm r _ (SDefaultCase e) = do
    emit "*)"
    nest $
      emitExp tm r e
emitCase tm r _ (SConstCase t e) = do
    emit $ show t ++ ")"
    nest $
      emitExp tm r e
emitCase tm r v (SConCase i0 t _ ns0 e) = do
    emit $ show t ++ ")"
    nest $ do
      emitCaseElement i0 ns0
      emitExp tm r e
  where
    emitCaseElement :: Int -> [Name] -> Emitter
    emitCaseElement _ []       = skip
    emitCaseElement i (_ : ns) = do
        emit $ showLoc i ++ "=${_A[" ++ showVar v ++ " + " ++ show (i - i0 + 1) ++ "]}"
        emitCaseElement (i + 1) ns


showName :: Name -> String
showName n =
    "idris_" ++ concatMap char (showCG n)
  where
    char x | isAlpha x || isDigit x = [x]
           | otherwise              = "_" ++ show (fromEnum x) ++ "_"


showLoc :: Int -> String
showLoc 0 = "_S[_SP]"
showLoc i = "_S[_SP + " ++ show i ++ "]"

showVar :: LVar -> String
showVar (Loc i) = showLoc i
showVar x       = error $ "Variable " ++ show x ++ " is not supported"

showParamVar :: LVar -> String
showParamVar x = "${" ++ showVar x ++ "}"

showQuotedParamVar :: LVar -> String
showQuotedParamVar x = "\"" ++ showParamVar x ++ "\""


showConst :: Const -> String
showConst (I i)             = show i
showConst (BI i)            = show i
-- showConst (Ch c)
showConst (Str s)           = "'" ++ s ++ "'"
showConst TheWorld          = "0"
showConst x | isTypeConst x = "0"
            | otherwise     = error $ "Constant " ++ show x ++ " is not supported"


showOp :: String -> PrimFn -> [LVar] -> String
showOp r (LPlus  (ATInt _)) [n, m] = r ++ "=$(( " ++ showVar n ++ " + "  ++ showVar m ++ " ))"
showOp r (LMinus (ATInt _)) [n, m] = r ++ "=$(( " ++ showVar n ++ " - "  ++ showVar m ++ " ))"
showOp r (LTimes (ATInt _)) [n, m] = r ++ "=$(( " ++ showVar n ++ " * "  ++ showVar m ++ " ))"
-- showOp r LUDiv
-- showOp r LSDiv
-- showOp r LURem
-- showOp r LSRem
-- showOp r LAnd
-- showOp r LOr
-- showOp r LXOr
-- showOp r LCompl
-- showOp r LSHL
-- showOp r LLSHR
-- showOp r LASHR
showOp r (LEq    (ATInt _)) [n, m] = r ++ "=$(( " ++ showVar n ++ " == " ++ showVar m ++ " ))"
-- showOp r LLt
-- showOp r LLe
-- showOp r LGt
-- showOp r LGe
showOp r (LSLt   (ATInt _)) [n, m] = r ++ "=$(( " ++ showVar n ++ " < "  ++ showVar m ++ " ))"
showOp r (LSLe   (ATInt _)) [n, m] = r ++ "=$(( " ++ showVar n ++ " <= " ++ showVar m ++ " ))"
showOp r (LSGt   (ATInt _)) [n, m] = r ++ "=$(( " ++ showVar n ++ " > "  ++ showVar m ++ " ))"
showOp r (LSGe   (ATInt _)) [n, m] = r ++ "=$(( " ++ showVar n ++ " >= " ++ showVar m ++ " ))"
showOp r (LSExt _ _)        [n]    = r ++ "=" ++ showParamVar n
-- showOp r LZExt
-- showOp r LTrunc
showOp r LStrConcat         [s, t] = r ++ "=" ++ showParamVar s ++ showParamVar t
-- showOp r LStrLt
-- showOp r LStrEq
showOp r LStrLen            [s]    = r ++ "=${#" ++  showVar s ++ "}"
-- showOp r LIntFloat
-- showOp r LFloatInt
showOp r (LIntStr _)        [n]    = r ++ "=" ++ showParamVar n
-- showOp r LStrInt
-- showOp r LFloatStr
-- showOp r LStrFloat
-- showOp r LChInt
-- showOp r LIntCh
-- showOp r LBitCast
-- showOp r LFExp
-- showOp r LFLog
-- showOp r LFSin
-- showOp r LFCos
-- showOp r LFTan
-- showOp r LFASin
-- showOp r LFACos
-- showOp r LFATan
-- showOp r LFSqrt
-- showOp r LFFloor
-- showOp r LFCeil
-- showOp r LFNegate
showOp r LStrHead           [s]    = r ++ "=${" ++ showVar s ++ ":0:1}"
showOp r LStrTail           [s]    = r ++ "=${" ++ showVar s ++ ":1}"
showOp r LStrCons           [c, s] = r ++ "=" ++ showParamVar c ++ showParamVar s
showOp r LStrIndex          [s, n] = r ++ "=${" ++ showVar s ++ ":" ++ showParamVar n ++ ":1}"
-- showOp r LStrRev
-- showOp r LReadStr
showOp _ LWriteStr          [_, s] = "echo " ++ showQuotedParamVar s
-- showOp r LSystemInfo
-- showOp r LFork
-- showOp r LPar
-- showOp r LExternal
-- showOp r LNoOp
-- showOp _ o _                       = error $ "Operator " ++ show o ++ " is not supported"
showOp _ o _                       = "echo 'Operator " ++ show o ++ " is not supported' >&2"
