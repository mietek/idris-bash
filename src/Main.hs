module Main where

import IRTS.CodegenBash
import IRTS.Compiler
import Idris.AbsSyntax
import Idris.ElabDecls
import Idris.REPL
import System.Environment
import System.Exit

data Opts = Opts
  { inputs :: [FilePath]
  , output :: FilePath
  }

showUsage :: IO ()
showUsage = do
    putStrLn "Usage: idris-bash <ibc-files> [-o <output-file>]"
    exitWith ExitSuccess

getOpts :: IO Opts
getOpts = do
    xs <- getArgs
    return $ reduce (Opts [] "a.sh") xs
  where
    reduce opts ("-o" : o : xs) = reduce (opts { output = o }) xs
    reduce opts (i : xs)        = reduce (opts { inputs = i : inputs opts }) xs
    reduce opts []              = opts

compileMain :: Opts -> Idris ()
compileMain opts = do
    elabPrims
    _ <- loadInputs (inputs opts) Nothing
    mainProg <- elabMain
    compileInfo <- compile (Via "bash") (output opts) (Just mainProg)
    runIO $ codegenBash compileInfo

main :: IO ()
main = do
    opts <- getOpts
    if (null (inputs opts))
      then showUsage
      else runMain (compileMain opts)
