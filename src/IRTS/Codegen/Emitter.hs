{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module IRTS.Codegen.Emitter
  ( emit
  , nest
  , skip
  , collect
  , Emitter
  , EmitterM
  ) where

import Control.Applicative
import Control.Monad
import Data.String


data EmitterM a = E a [ShowS]
  deriving Functor

instance Applicative EmitterM where
  pure  = return
  (<*>) = ap

instance Monad EmitterM where
  return a     = E a []
  E a xs >>= f =
      case f a of
        E b ys ->
            E b (xs ++ ys)


type Emitter = EmitterM ()

tab :: ShowS
tab = showString "\t"

emit :: String -> Emitter
emit s = E () [showString s]

nest :: Emitter -> Emitter
nest (E () xs) = E () (map (\x -> tab . x) xs)

skip :: Emitter
skip = return ()


instance (a ~ ()) => IsString (EmitterM a) where
  fromString s = emit s

collect :: Emitter -> String
collect (E () xs) = concatMap (\x -> x "\n") xs
