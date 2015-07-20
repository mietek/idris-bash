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
    mapM_ emitFun fs
    emit $ showName (sMN 0 "runMain")


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
        "_PSP[_F++]=${_SP}; " ++
        "_SP=${_SQ}; " ++
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
        "_SP=${_PSP[--_F]}"


emitExp :: SExp -> Emitter
emitExp (SV (Glob f))        = emitFunCall f []
emitExp (SV v@(Loc _))       = emitVar v
emitExp (SApp _ f vs)        = emitFunCall f vs
emitExp (SLet (Loc i) e1 e2) = do
    i' <- showLocTarget i
    withRetTarget i' $
      emitExp e1
    emitExp e2
-- emitExp (SUpdate _ e)
-- emitExp (SProj v i)
emitExp (SCon _ t _ vs)      = emitArray t vs
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


emitVar :: LVar -> Emitter
emitVar v = do
    v' <- showVar v
    rt <- askRetTarget
    if rt == v'
      then skip
      else do
        v'' <- showParamVar v
        emitRet v''


emitFunCall :: Name -> [LVar] -> Emitter
emitFunCall f vs = do
    vs' <- mapM showQuotedParamVar vs
    rt <- askRetTarget
    emit $ showSep " " (showName f : vs')
    if rt == "_R"
      then skip
      else emitRet "${_R}"


emitArray :: Int -> [LVar] -> Emitter
emitArray t [] = emitRet $ show t
emitArray t vs = do
    vs' <- mapM showParamVar vs
    emitRet $ "\"" ++ showSep " " (show t : map wrap vs') ++ "\""
  where
    wrap s = "(" ++ s ++ ")"


emitSwitch :: LVar -> [SAlt] -> Emitter
emitSwitch v cs = do
    v' <- showBareVar v
    cv <- if any isConCase cs
            then return $ "${" ++ v' ++ "%% *}"
            else showParamVar v
    emit $ "case " ++ cv ++ " in"
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
emitCase _ (SConCase _ t _ [] e) = do
    emit $ show t ++ ")"
    nest $
      emitExp e
emitCase v (SConCase i0 t _ ns e) = do
    emit $ show t ++ ")"
    nest $ do
      v' <- showQuotedParamVar v
      emit $ "IRTS_decodeArray " ++ v'
      emitProj i0
      emitExp e
  where
    emitProj i =
        if i == i0 + length ns
          then skip
          else do
            i' <- showLocTarget i
            withRetTarget i' $
              emitRet $ "${_A[" ++ show (i - i0 + 1) ++ "]}"
            emitProj (i + 1)


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
emitOp (LSExt _ _)        [n]      = emitVar n
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
emitOp (LIntStr _)        [n]      = emitVar n
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
