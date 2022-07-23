{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Main (main) where

import           Control.Monad.IO.Class   (MonadIO (liftIO))
import           System.Console.ANSI      (clearScreen, setCursorPosition)
import           System.Console.CmdArgs   (Data, Default (def), Typeable,
                                           cmdArgs, details, explicit, help,
                                           helpArg, name, program, summary,
                                           typFile, (&=))
import           System.Console.Haskeline (InputT, Settings (Settings),
                                           autoAddHistory, complete,
                                           getInputLine, historyFile,
                                           noCompletion, outputStrLn, runInputT)

data Args
  = Args
  { argsInput  :: String
  , argsOutput :: String
  , argsRepl   :: Bool
  }
  deriving (Show, Data, Typeable)

args :: Args
args
  = Args
  {  argsInput = def &= explicit &= name "i" &= name "input" &= help "Path of Hack Assembly file" &= typFile
  , argsOutput = def &= explicit &= name "o" &= name "output" &= help "Path for Hack Binary output" &= typFile
  ,   argsRepl = def &= explicit &= name "r" &= name "repl" &= help "Start a repl shell"
  }
  &= program "hasm"
  &= summary "Hack Assembler"
  &= details ["An Assembler for Hack Assembly.", "Produce hack binary <file_name>.hack"]
  &= helpArg [explicit, name "h", name "help"]


process :: String -> IO ()
process "" = putStr ""
process str = do
  let res = str
  putStrLn res

repl :: InputT IO ()
repl = do
  minput <- getInputLine "\ESC[35m>>>\ESC[0m " -- Magenta, ansi-terminal methods does not work with getInputLine


  case minput of
    Nothing       -> outputStrLn "Closing"
    Just "clear!" -> liftIO (clearScreen >> setCursorPosition 0 0) >> repl
    Just input    -> liftIO (process input) >> repl

replLoop :: IO ()
replLoop
  =  clearScreen
  >> setCursorPosition 0 0
  >> runInputT Settings
    { historyFile = Nothing
    , autoAddHistory = True
    , complete = noCompletion
    } repl


replaceExt :: String -> String -> String
replaceExt x = ((reverse . dropWhile (/= '.') $ reverse x) <>)

main :: IO ()
main = do
  progArgs@Args {argsInput=input} <- cmdArgs args

  if null input || argsRepl progArgs
    then replLoop
    else putStrLn $ replaceExt input "hack"
