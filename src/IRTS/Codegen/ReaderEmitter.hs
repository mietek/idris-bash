module IRTS.Codegen.ReaderEmitter
  ( Emitter
  , EmitterM
  , askPrelude
  , askFuns
  , askArgCount
  , askLocCount
  , askRetTarget
  , emit
  , nest
  , skip
  , collect
  , withProgInfo
  , withFunInfo
  , withRetTarget
  ) where

import Control.Monad.Reader (ReaderT, ask, lift, local, runReaderT)
import IRTS.CodegenCommon (CodegenInfo, simpleDecls)
import qualified IRTS.Codegen.Emitter as E
import IRTS.Codegen.Utils (countLocs)
import IRTS.Simplified (SDecl(..))
import Idris.Core.TT (Name)


data EmitterInfo = EI
  { eiPrelude   :: String
  , eiFuns      :: [(Name, SDecl)]
  , eiArgCount  :: Int
  , eiLocCount  :: Int
  , eiRetTarget :: String
  }

type EmitterM a = ReaderT EmitterInfo E.EmitterM a

askPrelude :: EmitterM String
askPrelude = fmap eiPrelude ask

askFuns :: EmitterM [(Name, SDecl)]
askFuns = fmap eiFuns ask

askArgCount :: EmitterM Int
askArgCount = fmap eiArgCount ask

askLocCount :: EmitterM Int
askLocCount = fmap eiLocCount ask

askRetTarget :: EmitterM String
askRetTarget = fmap eiRetTarget ask


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

collect :: Emitter -> String
collect e = E.collect (runEmitter ei e)
  where
    ei = EI { eiPrelude   = ""
            , eiFuns      = []
            , eiArgCount  = 0
            , eiLocCount  = 0
            , eiRetTarget = ""
            }

withProgInfo :: String -> CodegenInfo -> Emitter -> Emitter
withProgInfo p ci e =
    local progInfo e
  where
    fs = simpleDecls ci
    progInfo ei = ei { eiPrelude = p
                     , eiFuns    = fs
                     }

withFunInfo :: SDecl -> Emitter -> Emitter
withFunInfo f@(SFun _ args _ _) e =
    local funInfo $
      nest e
  where
    funInfo ei = ei { eiArgCount  = length args
                    , eiLocCount  = countLocs f
                    , eiRetTarget = "_R"
                    }

withRetTarget :: String -> Emitter -> Emitter
withRetTarget at e = local (\ei -> ei { eiRetTarget = at }) e
