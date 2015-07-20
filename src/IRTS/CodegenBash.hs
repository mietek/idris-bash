module IRTS.CodegenBash (codegenBash) where

import Data.Char (isAlpha, isDigit)
import Data.List (intersperse)
import IRTS.CodegenCommon (CodeGenerator, outputFile)
import IRTS.Codegen.ReaderEmitter
import IRTS.Lang (PrimFn(..), LVar(..))
import IRTS.Simplified (SAlt(..), SDecl(..), SExp(..))
import Idris.Core.TT (ArithTy(..), Const(..), Name, isTypeConst, showCG, showSep, sMN)

import Paths_idris_bash (getDataFileName)


codegenBash :: CodeGenerator
codegenBash ci = do
    pPath <- getDataFileName "prelude.sh"
    p <- readFile pPath
    let oPath = outputFile ci
        o = collect $
          withProgInfo p ci $
            emitProg
    writeFile oPath o


emitProg :: Emitter
emitProg = do
    p <- askPrelude
    fs <- askFuns
    emit p
    emit ""
    emitTags
    mapM_ emitFun fs
    emit $ showName (sMN 0 "runMain")


emitTags :: Emitter
emitTags = do
    ts <- askTags
    tc <- askTagCount
    mapM_ emitTag ts
    emit $ "_AP=" ++ show tc
    emit ""
    emit ""

emitTag :: (Int, Int) -> Emitter
emitTag (t, i) =
    emit $ "_A[" ++ show i ++ "]=" ++ show t


emitFun :: (Name, SDecl) -> Emitter
emitFun (n, f@(SFun _ _ _ e)) = do
    emit $ showName n ++ " () {"
    withFunInfo f $ do
      emitPushFrame
      emitExp e
      emitPopFrame
    emit "}"
    emit ""
    emit ""

emitPushFrame :: Emitter
emitPushFrame = do
    ac <- askArgCount
    lc <- askLocCount
    let fs = max ac lc
    if fs - ac == 0
      then skip
      else emit $
        "_PSP[_SR]=${_SP}; " ++
        "_SP=${_SQ}; " ++
        "_SR=$(( _SR + 1 )); " ++
        "_SQ=$(( _SP + " ++ show (lc - ac) ++ " ))"

emitPopFrame :: Emitter
emitPopFrame = do
    ac <- askArgCount
    lc <- askLocCount
    let fs = max ac lc
    if fs - ac == 0
      then skip
      else emit $
        "_SQ=${_SP}; " ++
        "_SR=$(( _SR - 1 )); " ++
        "_SP=${_PSP[_SR]}"


emitExp :: SExp -> Emitter
emitExp (SV (Glob f))        = emitFunCall f []
emitExp (SV v@(Loc _))       = showParamVar v >>= emitRet
emitExp (SApp _ f vs)        = emitFunCall f vs
emitExp (SLet (Loc i) e1 e2) = do
    i' <- showLocTarget i
    withRetTarget i' $
      emitExp e1
    emitExp e2
-- emitExp (SUpdate _ e)
-- emitExp (SProj v i)
emitExp (SCon _ t _ []) = do
    ap <- askTag t
    emitRet $ show ap
emitExp (SCon _ t _ vs) = do
    vs' <- mapM showParamVar vs
    emitArray (show t : vs')
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
    vs' <- mapM showQuotedParamVar vs
    rt <- askRetTarget
    emit $ showSep " " (showName f : vs')
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
    ap <- showVar v
    cv <- if any isConCase cs
            then return $ "${_A[" ++ ap ++ "]}"
            else showParamVar v
    emit $ "case " ++ cv ++ " in"
    sequence_ $
      intersperse (nest $ emit ";;") $
        map (emitCase ap) cs
    emit "esac"
  where
    isConCase (SConCase _ _ _ _ _) = True
    isConCase _                    = False

emitCase :: String -> SAlt -> Emitter
emitCase _ (SDefaultCase e) = do
    emit "*)"
    nest $
      emitExp e
emitCase _ (SConstCase t e) = do
    emit $ show t ++ ")"
    nest $
      emitExp e
emitCase ap (SConCase i0 t _ ns0 e) = do
    emit $ show t ++ ")"
    nest $ do
      emitCaseElement i0 ns0
      emitExp e
  where
    emitCaseElement :: Int -> [Name] -> Emitter
    emitCaseElement _ []       = skip
    emitCaseElement i (_ : ns) = do
        i' <- showLocTarget i
        withRetTarget i' $
          emitRet $ "${_A[" ++ ap ++ " + " ++ show (i - i0 + 1) ++ "]}"
        emitCaseElement (i + 1) ns


showName :: Name -> String
showName n =
    "idris_" ++ concatMap char (showCG n)
  where
    char x | isAlpha x || isDigit x = [x]
           | otherwise              = "_" ++ show (fromEnum x) ++ "_"


showArgLoc :: Int -> String
showArgLoc i = show (i + 1)

showStackLoc :: Int -> String
showStackLoc 0 = "_S[_SP]"
showStackLoc i = "_S[_SP + " ++ show i ++ "]"


showLocTarget :: Int -> EmitterM String
showLocTarget i = do
    ac <- askArgCount
    if i < ac
      then error $ "Cannot assign to argument"
      else return $ showStackLoc (i - ac)

showBareVar :: LVar -> EmitterM String
showBareVar (Loc i) = do
    ac <- askArgCount
    if i < ac
      then return $ showArgLoc i
      else return $ showStackLoc (i - ac)
showBareVar v       = error $ "Variable " ++ show v ++ " is not supported"

showVar :: LVar -> EmitterM String
showVar (Loc i) = do
    ac <- askArgCount
    if i < ac
      then return $ "$" ++ showArgLoc i
      else return $ showStackLoc (i - ac)
showVar v       = error $ "Variable " ++ show v ++ " is not supported"

showParamVar :: LVar -> EmitterM String
showParamVar (Loc i) = do
    ac <- askArgCount
    if i < ac
      then return $ "$" ++ showArgLoc i
      else return $ "${" ++ showStackLoc (i - ac) ++ "}"
showParamVar v       = error $ "Variable " ++ show v ++ " is not supported"

showQuotedParamVar :: LVar -> EmitterM String
showQuotedParamVar v = do
    v' <- showParamVar v
    return $ "\"" ++ v' ++ "\""


showConst :: Const -> String
showConst (I i)             = show i
showConst (BI i)            = show i
-- showConst (Ch c)
showConst (Str s)           = "'" ++ s ++ "'"
showConst TheWorld          = "0"
showConst x | isTypeConst x = "0"
            | otherwise     = error $ "Constant " ++ show x ++ " is not supported"


emitOp :: PrimFn -> [LVar] -> Emitter
emitOp (LPlus  (ATInt _)) [n1, n2] = emitArithOp n1 "+" n2
emitOp (LMinus (ATInt _)) [n1, n2] = emitArithOp n1 "-" n2
emitOp (LTimes (ATInt _)) [n1, n2] = emitArithOp n1 "*" n2
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
emitOp (LEq    (ATInt _)) [n1, n2] = emitArithOp n1 "==" n2
-- emitOp LLt
-- emitOp LLe
-- emitOp LGt
-- emitOp LGe
emitOp (LSLt   (ATInt _)) [n1, n2] = emitArithOp n1 "<" n2
emitOp (LSLe   (ATInt _)) [n1, n2] = emitArithOp n1 "<=" n2
emitOp (LSGt   (ATInt _)) [n1, n2] = emitArithOp n1 ">" n2
emitOp (LSGe   (ATInt _)) [n1, n2] = emitArithOp n1 ">=" n2
emitOp (LSExt _ _)        [n]      = showParamVar n >>= emitRet
-- emitOp LZExt
-- emitOp LTrunc
emitOp LStrConcat         [s1, s2] = do
    s1' <- showParamVar s1
    s2' <- showParamVar s2
    emitRet $ s1' ++ s2'
-- emitOp LStrLt
-- emitOp LStrEq
emitOp LStrLen            [s]      = do
    s' <- showBareVar s
    emitRet $ "${#" ++ s' ++ "}"
-- emitOp LIntFloat
-- emitOp LFloatInt
emitOp (LIntStr _)        [n]      = showParamVar n >>= emitRet
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
emitOp LStrHead           [s]    = do
    s' <- showBareVar s
    emitRet $ "${" ++ s' ++ ":0:1}"
emitOp LStrTail           [s]    = do
    s' <- showBareVar s
    emitRet $ "${" ++ s' ++ ":1}"
emitOp LStrCons           [c, s] = do
    c' <- showParamVar c
    s' <- showParamVar s
    emitRet $ c' ++ s'
emitOp LStrIndex          [s, n] = do
    s' <- showBareVar s
    n' <- showParamVar n
    emitRet $ "${" ++ s' ++ ":" ++ n' ++ ":1}"
-- emitOp LStrRev
-- emitOp LReadStr
emitOp LWriteStr          [_, s] = do
    s' <- showQuotedParamVar s
    emit $ "echo " ++ s'
-- emitOp LSystemInfo
-- emitOp LFork
-- emitOp LPar
-- emitOp LExternal
-- emitOp LNoOp
-- emitOp o _                       = error $ "Operator " ++ show o ++ " is not supported"
emitOp o _                       = emit $ "echo 'Operator " ++ show o ++ " is not supported' >&2"

emitArithOp :: LVar -> String -> LVar -> Emitter
emitArithOp n1 o n2 = do
    n1' <- showVar n1
    n2' <- showVar n2
    emitRet $ "$(( " ++ n1' ++ " " ++ o ++ " " ++ n2' ++ " ))"
