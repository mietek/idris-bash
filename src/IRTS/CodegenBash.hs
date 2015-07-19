module IRTS.CodegenBash (codegenBash) where

import Data.Char
import Data.List
import IRTS.CodegenCommon
import IRTS.CodegenEmitter
import IRTS.CodegenUtils
import IRTS.Lang
import IRTS.Simplified
import Idris.Core.TT

import Paths_idris_bash


codegenBash :: CodeGenerator
codegenBash ci = do
    preludeName <- getDataFileName "prelude.sh"
    prelude <- readFile preludeName
    let outputName = outputFile ci
        tm = findTags ci
        fs = simpleDecls ci
        output = collect $ do
            emit prelude
            emit ""
            cgTags tm
            mapM_ (cgFun tm) fs
            emit $ name (sMN 0 "runMain")
    writeFile outputName output


name :: Name -> String
name n =
    "idris_" ++ concatMap char (showCG n)
  where
    char x | isAlpha x || isDigit x = [x]
           | otherwise              = "_" ++ show (fromEnum x) ++ "_"


loc :: Int -> String
loc 0 = "_S[_SP]"
loc i = "_S[_SP + " ++ show i ++ "]"


cgVar :: LVar -> String
cgVar (Loc i) = loc i
cgVar x       = error $ "Variable " ++ show x ++ " is not supported"


cgPVar :: LVar -> String
cgPVar x = "${" ++ cgVar x ++ "}"


cgQPVar :: LVar -> String
cgQPVar x = "\"" ++ cgPVar x ++ "\""


cgTags :: TagMap -> Emitter
cgTags tm = do
    mapM_ cgTag (askTags tm)
    emit $ "_AP=" ++ show (askTagCount tm)
    emit ""
    emit ""


cgTag :: (Int, Int) -> Emitter
cgTag (t, i) =
    emit $ "_A[" ++ show i ++ "]=" ++ show t


cgFun :: TagMap -> (Name, SDecl) -> Emitter
cgFun tm (n, f@(SFun _ args _ e)) = do
    emit $ name n ++ " () {"
    nest $ do
      cgPushFrame fs
      mapM_ cgMoveArg [1..ac]
      cgSizeFrame fs
      cgBody tm "_R" e
      cgPopFrame fs
    emit "}"
    emit ""
    emit ""
  where
    ac = length args
    lc = countLocs f
    fs = max ac lc


cgPushFrame :: Int -> Emitter
cgPushFrame 0 = skip
cgPushFrame _ = emit $ "_PSP[_SR]=${_SP}; _SP=${_SQ}; _SR=$(( _SR + 1 ))"


cgMoveArg :: Int -> Emitter
cgMoveArg 1 = emit $ "_S[_SP]=$1"
cgMoveArg i = emit $ "_S[_SP + " ++ show (i - 1) ++ "]=$" ++ show i


cgSizeFrame :: Int -> Emitter
cgSizeFrame 0  = skip
cgSizeFrame fs = emit $ "_SQ=$(( _SP + " ++ show fs ++ " ))"


cgPopFrame :: Int -> Emitter
cgPopFrame 0 = skip
cgPopFrame _ = emit $ "_SQ=${_SP}; _SR=$(( _SR - 1 )); _SP=${_PSP[_SR]}"


cgBody :: TagMap -> String -> SExp -> Emitter
cgBody _  r (SV (Glob f))        = cgFunCall r f []
cgBody _  r (SV v@(Loc _))       = emit $ r ++ "=" ++ cgPVar v
cgBody _  r (SApp _ f vs)        = cgFunCall r f vs
cgBody tm r (SLet (Loc i) e1 e2) = do
    cgBody tm (loc i) e1
    cgBody tm r e2
-- cgBody tm r (SUpdate _ e)
-- cgBody tm r (SProj v i)
cgBody tm r (SCon _ t _ [])      = emit $ r ++ "=" ++ show (askTag tm t)
cgBody _  r (SCon _ t _ vs)      = cgArray r (show t : map cgPVar vs)
cgBody tm r (SCase _ v cs)       = cgSwitch tm r v cs
cgBody tm r (SChkCase v cs)      = cgSwitch tm r v cs
cgBody _  r (SConst c)           = emit $ r ++ "=" ++ cgConst c
cgBody _  r (SOp o vs)           = emit $ cgOp r o vs
cgBody _  r SNothing             = emit $ r ++ "=0"
-- cgBody tm r (SError x)
cgBody _  _ x                    = error $ "Expression " ++ show x ++ " is not supported"


cgFunCall :: String -> Name -> [LVar] -> Emitter
cgFunCall r f vs = do
    emit $ showSep " " (name f : map cgQPVar vs)
    cgFunRet r


cgFunRet :: String -> Emitter
cgFunRet "_R" = skip
cgFunRet r    = emit $ r ++ "=${_R}"


cgArray :: String -> [String] -> Emitter
cgArray r args = do
    mapM_ cgArrayElement (zip [0..] args)
    emit $ r ++ "=${_AP}"
    cgPushArray ac
  where
    ac = length args


cgArrayElement :: (Int, String) -> Emitter
cgArrayElement (0, arg) = emit $ "_A[_AP]=" ++ arg
cgArrayElement (i, arg) = emit $ "_A[_AP + " ++ show i ++ "]=" ++ arg


cgPushArray :: Int -> Emitter
cgPushArray 0  = skip
cgPushArray ac = emit $ "_AP=$(( _AP + " ++ show ac ++ " ))"


cgSwitch :: TagMap -> String -> LVar -> [SAlt] -> Emitter
cgSwitch tm r v cs = do
    let v' = if any isConCase cs then "${_A[" ++ cgVar v ++ "]}" else cgPVar v
    emit $ "case " ++ v' ++ " in"
    sequence_ $
      intersperse (nest $ emit ";;") $
        map (cgCase tm r v) cs
    emit "esac"
  where
    isConCase (SConCase _ _ _ _ _) = True
    isConCase _                    = False


cgCase :: TagMap -> String -> LVar -> SAlt -> Emitter
cgCase tm r _ (SDefaultCase e) = do
    emit "*)"
    nest $
      cgBody tm r e
cgCase tm r _ (SConstCase t e) = do
    emit $ show t ++ ")"
    nest $
      cgBody tm r e
cgCase tm r v (SConCase i0 t _ ns0 e) = do
    emit $ show t ++ ")"
    nest $ do
      cgCaseElement i0 ns0
      cgBody tm r e
  where
    cgCaseElement :: Int -> [Name] -> Emitter
    cgCaseElement _ []       = skip
    cgCaseElement i (_ : ns) = do
        emit $ loc i ++ "=${_A[" ++ cgVar v ++ " + " ++ show (i - i0 + 1) ++ "]}"
        cgCaseElement (i + 1) ns


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
