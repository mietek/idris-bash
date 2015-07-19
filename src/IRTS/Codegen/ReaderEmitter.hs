module IRTS.Codegen.ReaderEmitter
  ( askPrelude
  , askTagMap
  , askTags
  , askTagCount
  , askTag
  , emit
  , nest
  , skip
  , collect
  , Emitter
  ) where

import Control.Monad.Reader
import qualified Data.IntMap.Strict as M
import IRTS.CodegenCommon
import qualified IRTS.Codegen.Emitter as E
import IRTS.Codegen.Utils


data EmitterInfo = EI
  { eiPrelude      :: String
  , eiTagMap       :: TagMap
  }

type EmitterM a = ReaderT EmitterInfo E.EmitterM a

askPrelude :: EmitterM String
askPrelude = fmap eiPrelude ask

askTagMap :: EmitterM TagMap
askTagMap = fmap eiTagMap ask

askTags :: EmitterM [(Int, Int)]
askTags = do
    tm <- askTagMap
    return $ M.toAscList tm

askTagCount :: EmitterM Int
askTagCount = do
    tm <- askTagMap
    return $ M.size tm

askTag :: Int -> EmitterM Int
askTag t = do
    tm <- askTagMap
    return $ tm M.! t


type Emitter = EmitterM ()

runEmitter :: EmitterInfo -> Emitter -> E.Emitter
runEmitter ei e = runReaderT e ei

emit :: String -> Emitter
emit s = lift $ E.emit s

nest :: Emitter -> Emitter
nest e = do
    ei <- ask
    lift $ E.nest $ runEmitter ei e

skip :: Emitter
skip = lift E.skip

collect :: String -> CodegenInfo -> Emitter -> String
collect prelude ci e = E.collect (runEmitter ei e)
  where
    tm = findTags ci
    ei = EI prelude tm
