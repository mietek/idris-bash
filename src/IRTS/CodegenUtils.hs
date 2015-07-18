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


askTag :: TagMap -> Int -> Int
askTag tm t = tm M.! t


askTags :: TagMap -> [(Int, Int)]
askTags tm = M.toAscList tm


askTagCount :: TagMap -> Int
askTagCount tm = M.size tm


countLocs :: SDecl -> Int
countLocs (SFun _ _ _ e) = clExp e


clExp :: SExp -> Int
clExp (SV (Loc i))         = i + 1
clExp (SLet (Loc i) e1 e2) = max (i + 1) (max (clExp e1) (clExp e2))
clExp (SCase _ _ cs)       = maximum (map clCase cs)
clExp (SChkCase _ cs)      = maximum (map clCase cs)
clExp _                    = 0


clCase :: SAlt -> Int
clCase (SDefaultCase e)      = clExp e
clCase (SConstCase _ e)      = clExp e
clCase (SConCase _ _ _ [] e) = clExp e
clCase (SConCase i _ _ ns e) = max (i + length ns) (clExp e)
