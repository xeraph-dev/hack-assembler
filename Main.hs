{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use takeWhile" #-}

module Main (main) where

import qualified Data.Char           as C
import qualified Data.List           as L
import qualified Data.List.Split     as L
import qualified Data.Map            as M
import qualified Data.Maybe          as Maybe
import           System.Console.ANSI (Color (Green, Red, Yellow),
                                      ColorIntensity (Vivid),
                                      ConsoleLayer (Foreground),
                                      SGR (Reset, SetColor), setSGR)
import           System.Directory    (doesFileExist)
import           System.Environment  (getArgs, getProgName)
import           System.Exit         (exitFailure, exitSuccess)

newtype Args = Args {
  asmPath :: FilePath
} deriving (Show)

type Addresses = M.Map String Int
type Vars = M.Map String Int

setArgsAsmPath :: Args -> FilePath -> Args
setArgsAsmPath _ asmPath' = Args{asmPath=asmPath'}


dests :: String -> String
dests ""    = "000"
dests "0"   = "000"
dests "M"   = "001"
dests "D"   = "010"
dests "MD"  = "011"
dests "A"   = "100"
dests "AM"  = "101"
dests "AD"  = "110"
dests "AMD" = "111"
dests _     = error "Bad dest"


jumps :: String -> String
jumps ""     = "000"
jumps "null" = "000"
jumps "JGT"  = "001"
jumps "JEQ"  = "010"
jumps "JGE"  = "011"
jumps "JLT"  = "100"
jumps "JNE"  = "101"
jumps "JLE"  = "110"
jumps "JMP"  = "111"
jumps _      = error "Bad jump"


addresses :: Addresses
addresses =
  M.fromList [("SP",         0)
             ,("LCL",        1)
             ,("ARG",        2)
             ,("THIS",       3)
             ,("THAT",       4)
             ,("R0",         0)
             ,("R1",         1)
             ,("R2",         2)
             ,("R3",         3)
             ,("R4",         4)
             ,("R5",         5)
             ,("R6",         6)
             ,("R7",         7)
             ,("R8",         8)
             ,("R9",         9)
             ,("R10",       10)
             ,("R11",       11)
             ,("R12",       12)
             ,("R13",       13)
             ,("R14",       14)
             ,("R15",       15)
             ,("SCREEN", 16384)
             ,("KBD",    24576)]

comps :: String -> (String, String)
comps ""    = ("0", "000000")
comps "0"   = ("0", "101010")
comps "1"   = ("0", "111111")
comps "-1"  = ("0", "111010")
comps "D"   = ("0", "001100")
comps "A"   = ("0", "110000")
comps "!D"  = ("0", "001101")
comps "!A"  = ("0", "110001")
comps "-D"  = ("0", "001111")
comps "-A"  = ("0", "110011")
comps "D+1" = ("0", "011111")
comps "A+1" = ("0", "110111")
comps "D-1" = ("0", "001110")
comps "A-1" = ("0", "110010")
comps "D+A" = ("0", "000010")
comps "D-A" = ("0", "010011")
comps "A-D" = ("0", "000111")
comps "D&A" = ("0", "000000")
comps "D|A" = ("0", "010101")
comps "M"   = ("1", "110000")
comps "!M"  = ("1", "110001")
comps "-M"  = ("1", "110011")
comps "M+1" = ("1", "110111")
comps "M-1" = ("1", "110010")
comps "D+M" = ("1", "000010")
comps "D-M" = ("1", "010011")
comps "M-D" = ("1", "000111")
comps "D&M" = ("1", "000000")
comps "D|M" = ("1", "010101")
comps _     = error "Bad comp"


fillA :: String -> String
fillA x = case length x of
  16 -> x
  _  -> fillA $ "0" ++ x


bin :: Int -> String
bin x
  | x <= 0 = ""
  | otherwise = bin (div x 2) ++ show (mod x 2)


printHelp :: IO ()
printHelp = do
  name <- getProgName
  putStrLn name
  putStr "Usage: hasm "
  setSGR [SetColor Foreground Vivid Yellow]
  putStrLn "<file_name>.asm"
  setSGR [Reset]
  putStr "Produce a hack binary file "
  setSGR [SetColor Foreground Vivid Yellow]
  putStrLn "<file_name>.hack"
  setSGR [Reset]


printFileMissing :: IO ()
printFileMissing = do
  printHelp
  setSGR [SetColor Foreground Vivid Red]
  putStrLn "\nFile missing"
  setSGR [Reset]


parseAsmPath :: FilePath -> Bool -> forall . IO FilePath
parseAsmPath file exists
          | null file || all C.isSpace file = printFileMissing >> exitFailure
          | not exists = do
              putStr "File "
              setSGR [SetColor Foreground Vivid Yellow]
              putStr file
              setSGR [Reset]
              putStrLn " does not exists"
              exitFailure
          | exists && ".asm" `L.isSuffixOf` file = return file
          | otherwise =  do
              putStr "File "
              setSGR [SetColor Foreground Vivid Yellow]
              putStr file
              setSGR [Reset]
              putStr " does not have "
              setSGR [SetColor Foreground Vivid Green]
              putStr "asm"
              setSGR [Reset]
              putStrLn " extension"
              exitFailure


parseArgs :: Args -> [String] -> IO Args
parseArgs _ ("-h":_) = printHelp >> exitSuccess
parseArgs args ("--help":xs) = parseArgs args ("-h":xs)
parseArgs args (file:xs) = do
    asmPath' <- parseAsmPath file =<< doesFileExist file
    flip parseArgs xs $ setArgsAsmPath args asmPath'
parseArgs args []
        | null (asmPath args) || all C.isSpace (asmPath args) = printFileMissing >> exitFailure
        | otherwise = return args


parser :: [String] -> (Vars, [String])
parser a =
  let parser' :: Vars -> [String] -> [String] -> (Vars, [String])
      parser' vars [] acc               = (vars, reverse acc)
      parser' vars (('/':'/':_):xs) acc = parser' vars xs acc
      parser' vars (('(':s):xs) acc = parser' (M.insert  (fst $ break (== ')') s) (length acc) vars) xs acc
      parser' vars (x:xs) acc
                | null x || all C.isSpace x = parser' vars xs acc
                | otherwise = parser' vars xs $ head (L.splitOn "//" [ c | c <- x, not $ C.isSpace c]):acc

  in parser' M.empty a []


eval :: (Vars, [String]) -> [String]
eval (vs, as) =
  let eval' :: Vars -> Int -> [String] -> [String] -> [String]
      eval' _ _ [] acc           = reverse acc
      eval' vars curr (('@':s):xs) acc
              | all C.isDigit s = eval' vars curr xs ((fillA . bin $ read s):acc)
              | Maybe.isJust $ M.lookup s addresses = eval' vars curr xs ((fillA . bin $ addresses M.! s):acc)
              | Maybe.isJust $ M.lookup s vars = eval' vars curr xs ((fillA . bin $ vars M.! s):acc)
              | otherwise = eval' (M.insert s curr vars) (curr + 1) xs (fillA (bin curr):acc)
      eval' vars curr (x:xs) acc = eval' vars curr xs (addr:acc)
        where
          dest | '=' `elem` x = dests $ fst (break (== '=') x)
               | otherwise = dests ""
          jump | ';' `elem` x = jumps $ dropWhile (== ';') (dropWhile (/= ';') x)
               | otherwise = dests ""
          (a, comp)
               | '=' `elem` x  = comps $ dropWhile (== '=') $ fst (break (== ';') $ dropWhile (/= '=') x)
               | notElem '=' x = comps $ fst (break (== ';') x)
               | otherwise = comps ""
          addr = "111" ++ a ++ comp ++ dest ++ jump
  in eval' vs 16 as []


replaceExt :: String -> String
replaceExt x = (reverse . head $ tail (L.splitOn "msa." $ reverse x)) ++ ".hack"


main :: IO ()
main = do
  args <- parseArgs Args { asmPath="" } =<< getArgs

  asmFile <- readFile $ asmPath args

  writeFile (replaceExt $ asmPath args) . unlines . eval . parser $ lines asmFile
