module HackAssembler.Assembler (assemble) where

import           Data.ByteString.Lazy.Char8 as BS
import           HackAssembler.AST          (AInstr (..), CInstr (..),
                                             Comp (..), Dest (..), Exp (..),
                                             Jump (..))
import           HackAssembler.Eval         (eval)

fillA :: ByteString -> ByteString
fillA x = case BS.length x of
  16 -> x
  _  -> fillA $ "0" <> x


bin :: Integer -> ByteString
bin x
  | x <= 0 = ""
  | otherwise = bin (div x 2) <> BS.pack (show (mod x 2))

b2s :: Bool -> ByteString
b2s x = BS.pack . show $ fromEnum x

assembleDest :: Dest -> ByteString
assembleDest (Dest d1 d2 d3) = b2s d1 <> b2s d2 <> b2s d3

assembleComp :: Comp -> ByteString
assembleComp (Comp c1 c2 c3 c4 c5 c6) = b2s c1 <> b2s c2 <> b2s c3 <> b2s c4 <> b2s c5 <> b2s c6

assembleJump :: Jump -> ByteString
assembleJump (Jump j1 j2 j3) = b2s j1 <> b2s j2 <> b2s j3

assembleExp :: Exp a -> [ByteString] -> [ByteString]
assembleExp (EAInstr _ (Const c)) acc                 = fillA (bin c):acc
assembleExp (ECInstr _ (CInstr a dest comp jump)) acc = "111" <> b2s a<> assembleComp comp <> assembleDest dest  <> assembleJump jump:acc
assembleExp _ _                                       = error "Assembler error: Bad instruction"

assemble :: ByteString -> Either String ByteString
assemble code = case eval code of
                  Left e -> Left e
                  Right p -> Right . BS.unlines . Prelude.reverse $ Prelude.foldl (flip assembleExp) [] p
