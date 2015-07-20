module IRTS.Codegen.Utils where

import IRTS.Lang (LVar(..))
import IRTS.Simplified (SAlt(..), SDecl(..), SExp(..))


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
