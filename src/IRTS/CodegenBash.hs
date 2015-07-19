module IRTS.CodegenBash (codegenBash) where

import Data.Char
import Data.List
import IRTS.CodegenCommon
import IRTS.Codegen.ReaderEmitter
import IRTS.Codegen.Utils
import IRTS.Lang
import IRTS.Simplified
import Idris.Core.TT

import Paths_idris_bash


codegenBash :: CodeGenerator
codegenBash ci = do
    preludePath <- getDataFileName "prelude.sh"
    prelude <- readFile preludePath
    let fs = simpleDecls ci
        outputPath = outputFile ci
        output = collect prelude ci $ emitProgram fs
    writeFile outputPath output


emitProgram :: [(Name, SDecl)] -> Emitter
emitProgram fs = do
    prelude <- askPrelude
    emit prelude
    emit ""
    emitTagDefs
    mapM_ emitFunDef fs
    emit $ showName (sMN 0 "runMain")


emitTagDefs :: Emitter
emitTagDefs = do
    ts <- askTags
    tc <- askTagCount
    mapM_ emitTagDef ts
    emit $ "_AP=" ++ show tc
    emit ""
    emit ""

emitTagDef :: (Int, Int) -> Emitter
emitTagDef (t, i) =
    emit $ "_A[" ++ show i ++ "]=" ++ show t


emitFunDef :: (Name, SDecl) -> Emitter
emitFunDef (n, f@(SFun _ args _ e)) = do
    emit $ showName n ++ " () {"
    nest $ do
      emitPushFrame fs
      mapM_ emitMoveArg [1..ac]
      emitSizeFrame fs
      withRetTarget "_R" $
        emitExp e
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


emitExp :: SExp -> Emitter
emitExp (SV (Glob f))        = emitFunCall f []
emitExp (SV v@(Loc _))       = emitRet $ showParamVar v
emitExp (SApp _ f vs)        = emitFunCall f vs
emitExp (SLet (Loc i) e1 e2) = do
    withRetTarget (showLoc i) $
      emitExp e1
    emitExp e2
-- emitExp (SUpdate _ e)
-- emitExp (SProj v i)
emitExp (SCon _ t _ []) = do
    t' <- askTag t
    emitRet $ show t'
emitExp (SCon _ t _ vs)      = emitArray (show t : map showParamVar vs)
emitExp (SCase _ v cs)       = emitSwitch v cs
emitExp (SChkCase v cs)      = emitSwitch v cs
emitExp (SConst c)           = emitRet $ showConst c
emitExp (SOp o vs)           = emitOp o vs
emitExp SNothing             = emitRet "0"
-- emitExp (SError x)
emitExp x                    = error $ "Expression " ++ show x ++ " is not supported"


emitRet :: String -> Emitter
emitRet s = do
    rt <- askRetTarget
    emit $ rt ++ "=" ++ s


emitFunCall :: Name -> [LVar] -> Emitter
emitFunCall f vs = do
    rt <- askRetTarget
    emit $ showSep " " (showName f : map showQuotedParamVar vs)
    if rt == "_R"
      then skip
      else emit $ rt ++ "=${_R}"


emitArray :: [String] -> Emitter
emitArray args = do
    mapM_ emitArrayElement (zip [0..] args)
    emitRet "${_AP}"
    emitPushArray ac
  where
    ac = length args

emitArrayElement :: (Int, String) -> Emitter
emitArrayElement (0, arg) = emit $ "_A[_AP]=" ++ arg
emitArrayElement (i, arg) = emit $ "_A[_AP + " ++ show i ++ "]=" ++ arg

emitPushArray :: Int -> Emitter
emitPushArray 0  = skip
emitPushArray ac = emit $ "_AP=$(( _AP + " ++ show ac ++ " ))"


emitSwitch :: LVar -> [SAlt] -> Emitter
emitSwitch v cs = do
    let v' = if any isConCase cs then "${_A[" ++ showVar v ++ "]}" else showParamVar v
    emit $ "case " ++ v' ++ " in"
    sequence_ $
      intersperse (nest $ emit ";;") $
        map (emitCase v) cs
    emit "esac"
  where
    isConCase (SConCase _ _ _ _ _) = True
    isConCase _                    = False

emitCase :: LVar -> SAlt -> Emitter
emitCase _ (SDefaultCase e) = do
    emit "*)"
    nest $
      emitExp e
emitCase _ (SConstCase t e) = do
    emit $ show t ++ ")"
    nest $
      emitExp e
emitCase v (SConCase i0 t _ ns0 e) = do
    emit $ show t ++ ")"
    nest $ do
      emitCaseElement i0 ns0
      emitExp e
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


emitOp :: PrimFn -> [LVar] -> Emitter
emitOp (LPlus  (ATInt _)) [n, m] = emitRet $ "$(( " ++ showVar n ++ " + "  ++ showVar m ++ " ))"
emitOp (LMinus (ATInt _)) [n, m] = emitRet $ "$(( " ++ showVar n ++ " - "  ++ showVar m ++ " ))"
emitOp (LTimes (ATInt _)) [n, m] = emitRet $ "$(( " ++ showVar n ++ " * "  ++ showVar m ++ " ))"
-- emitOp LUDiv
-- emitOp LSDiv
-- emitOp LURem
-- emitOp LSRem
-- emitOp LAnd
-- emitOp LOr
-- emitOp LXOr
-- emitOp LCompl
-- emitOp LSHL
-- emitOp LLSHR
-- emitOp LASHR
emitOp (LEq    (ATInt _)) [n, m] = emitRet $ "$(( " ++ showVar n ++ " == " ++ showVar m ++ " ))"
-- emitOp LLt
-- emitOp LLe
-- emitOp LGt
-- emitOp LGe
emitOp (LSLt   (ATInt _)) [n, m] = emitRet $ "$(( " ++ showVar n ++ " < "  ++ showVar m ++ " ))"
emitOp (LSLe   (ATInt _)) [n, m] = emitRet $ "$(( " ++ showVar n ++ " <= " ++ showVar m ++ " ))"
emitOp (LSGt   (ATInt _)) [n, m] = emitRet $ "$(( " ++ showVar n ++ " > "  ++ showVar m ++ " ))"
emitOp (LSGe   (ATInt _)) [n, m] = emitRet $ "$(( " ++ showVar n ++ " >= " ++ showVar m ++ " ))"
emitOp (LSExt _ _)        [n]    = emitRet $ showParamVar n
-- emitOp LZExt
-- emitOp LTrunc
emitOp LStrConcat         [s, t] = emitRet $ showParamVar s ++ showParamVar t
-- emitOp LStrLt
-- emitOp LStrEq
emitOp LStrLen            [s]    = emitRet $ "${#" ++ showVar s ++ "}"
-- emitOp LIntFloat
-- emitOp LFloatInt
emitOp (LIntStr _)        [n]    = emitRet $ showParamVar n
-- emitOp LStrInt
-- emitOp LFloatStr
-- emitOp LStrFloat
-- emitOp LChInt
-- emitOp LIntCh
-- emitOp LBitCast
-- emitOp LFExp
-- emitOp LFLog
-- emitOp LFSin
-- emitOp LFCos
-- emitOp LFTan
-- emitOp LFASin
-- emitOp LFACos
-- emitOp LFATan
-- emitOp LFSqrt
-- emitOp LFFloor
-- emitOp LFCeil
-- emitOp LFNegate
emitOp LStrHead           [s]    = emitRet $ "${" ++ showVar s ++ ":0:1}"
emitOp LStrTail           [s]    = emitRet $ "${" ++ showVar s ++ ":1}"
emitOp LStrCons           [c, s] = emitRet $ showParamVar c ++ showParamVar s
emitOp LStrIndex          [s, n] = emitRet $ "${" ++ showVar s ++ ":" ++ showParamVar n ++ ":1}"
-- emitOp LStrRev
-- emitOp LReadStr
emitOp LWriteStr          [_, s] = emit $ "echo " ++ showQuotedParamVar s
-- emitOp LSystemInfo
-- emitOp LFork
-- emitOp LPar
-- emitOp LExternal
-- emitOp LNoOp
-- emitOp o _                       = error $ "Operator " ++ show o ++ " is not supported"
emitOp o _                       = emit $ "echo 'Operator " ++ show o ++ " is not supported' >&2"
