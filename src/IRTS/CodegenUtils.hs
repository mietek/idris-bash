module IRTS.CodegenUtils where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M
import Data.List
import IRTS.CodegenCommon
import IRTS.Lang
import IRTS.Simplified
import Idris.Core.TT


type TagMap = IntMap Int


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
