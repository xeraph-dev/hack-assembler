module HackAssembler.ParserSpec (spec, parseW, ExpW(..)) where

import           Data.ByteString.Lazy.Char8 (ByteString)
import           HackAssembler.AST          (AInstr (..), CInstr (CInstr),
                                             Comp (..), Dest (..), Exp (..),
                                             Jump (..), emptyDest, emptyJump)
import           HackAssembler.Lexer        (Range)
import           HackAssembler.Parser       (parse)
import           Test.Hspec                 (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "A-Instruction" $ do
    it "constant"   $ parseW "@2"     `shouldBe` Right [EAInstrW (Const 2)]
    it "symbol"     $ parseW "@a_.:$" `shouldBe` Right [EAInstrW (Symbol "a_.:$")]

    it "symbol M"   $ parseW "@M"     `shouldBe` Right [EAInstrW (Symbol "M")]
    it "symbol D"   $ parseW "@D"     `shouldBe` Right [EAInstrW (Symbol "D")]
    it "symbol DM"  $ parseW "@DM"    `shouldBe` Right [EAInstrW (Symbol "DM")]
    it "symbol A"   $ parseW "@A"     `shouldBe` Right [EAInstrW (Symbol "A")]
    it "symbol AM"  $ parseW "@AM"    `shouldBe` Right [EAInstrW (Symbol "AM")]
    it "symbol AD"  $ parseW "@AD"    `shouldBe` Right [EAInstrW (Symbol "AD")]
    it "symbol ADM" $ parseW "@ADM"   `shouldBe` Right [EAInstrW (Symbol "ADM")]

    it "symbol JGT" $ parseW "@JGT"   `shouldBe` Right [EAInstrW (Symbol "JGT")]
    it "symbol JEQ" $ parseW "@JEQ"   `shouldBe` Right [EAInstrW (Symbol "JEQ")]
    it "symbol JGE" $ parseW "@JGE"   `shouldBe` Right [EAInstrW (Symbol "JGE")]
    it "symbol JLT" $ parseW "@JLT"   `shouldBe` Right [EAInstrW (Symbol "JLT")]
    it "symbol JNE" $ parseW "@JNE"   `shouldBe` Right [EAInstrW (Symbol "JNE")]
    it "symbol JLE" $ parseW "@JLE"   `shouldBe` Right [EAInstrW (Symbol "JLE")]
    it "symbol JMP" $ parseW "@JMP"   `shouldBe` Right [EAInstrW (Symbol "JMP")]

  describe "C-Instruction" $ do
    describe "dest" $ do
      it "  M=A"  $ parseW "M=A"    `shouldBe` Right [ECInstrW (CInstr False (Dest False  False True)   (Comp True True False False False False) emptyJump)]
      it "  D=A"  $ parseW "D=A"    `shouldBe` Right [ECInstrW (CInstr False (Dest False  True  False)  (Comp True True False False False False) emptyJump)]
      it " DM=A"  $ parseW "DM=A"   `shouldBe` Right [ECInstrW (CInstr False (Dest False  True  True)   (Comp True True False False False False) emptyJump)]
      it " MD=A"  $ parseW "MD=A"   `shouldBe` Right [ECInstrW (CInstr False (Dest False  True  True)   (Comp True True False False False False) emptyJump)]
      it "  A=A"  $ parseW "A=A"    `shouldBe` Right [ECInstrW (CInstr False (Dest True   False False)  (Comp True True False False False False) emptyJump)]
      it " AM=A"  $ parseW "AM=A"   `shouldBe` Right [ECInstrW (CInstr False (Dest True   False True)   (Comp True True False False False False) emptyJump)]
      it " MA=A"  $ parseW "MA=A"   `shouldBe` Right [ECInstrW (CInstr False (Dest True   False True)   (Comp True True False False False False) emptyJump)]
      it " AD=A"  $ parseW "AD=A"   `shouldBe` Right [ECInstrW (CInstr False (Dest True   True  False)  (Comp True True False False False False) emptyJump)]
      it " DA=A"  $ parseW "DA=A"   `shouldBe` Right [ECInstrW (CInstr False (Dest True   True  False)  (Comp True True False False False False) emptyJump)]
      it "ADM=A"  $ parseW "ADM=A"  `shouldBe` Right [ECInstrW (CInstr False (Dest True   True  True)   (Comp True True False False False False) emptyJump)]
      it "AMD=A"  $ parseW "AMD=A"  `shouldBe` Right [ECInstrW (CInstr False (Dest True   True  True)   (Comp True True False False False False) emptyJump)]
      it "DAM=A"  $ parseW "DAM=A"  `shouldBe` Right [ECInstrW (CInstr False (Dest True   True  True)   (Comp True True False False False False) emptyJump)]
      it "MAD=A"  $ parseW "MAD=A"  `shouldBe` Right [ECInstrW (CInstr False (Dest True   True  True)   (Comp True True False False False False) emptyJump)]
      it "DMA=A"  $ parseW "DMA=A"  `shouldBe` Right [ECInstrW (CInstr False (Dest True   True  True)   (Comp True True False False False False) emptyJump)]
      it "MDA=A"  $ parseW "MDA=A"  `shouldBe` Right [ECInstrW (CInstr False (Dest True   True  True)   (Comp True True False False False False) emptyJump)]

    describe "jump" $ do
      it "A;JGT"  $ parseW "A;JGT"  `shouldBe` Right [ECInstrW (CInstr False emptyDest (Comp True True False False False False) (Jump False False True))]
      it "A;JEQ"  $ parseW "A;JEQ"  `shouldBe` Right [ECInstrW (CInstr False emptyDest (Comp True True False False False False) (Jump False True  False))]
      it "A;JGE"  $ parseW "A;JGE"  `shouldBe` Right [ECInstrW (CInstr False emptyDest (Comp True True False False False False) (Jump False True  True))]
      it "A;JLT"  $ parseW "A;JLT"  `shouldBe` Right [ECInstrW (CInstr False emptyDest (Comp True True False False False False) (Jump True  False False))]
      it "A;JNE"  $ parseW "A;JNE"  `shouldBe` Right [ECInstrW (CInstr False emptyDest (Comp True True False False False False) (Jump True  False True))]
      it "A;JLE"  $ parseW "A;JLE"  `shouldBe` Right [ECInstrW (CInstr False emptyDest (Comp True True False False False False) (Jump True  True  False))]
      it "A;JMP"  $ parseW "A;JMP"  `shouldBe` Right [ECInstrW (CInstr False emptyDest (Comp True True False False False False) (Jump True  True  True))]

    describe "dest comp jump" $ do
      it "M=A;JGT"  $ parseW "M=A;JGT"  `shouldBe` Right [ECInstrW (CInstr False (Dest False False True) (Comp True True False False False False) (Jump False False True))]

    describe "comp" $ do
      it "A"  $ parseW "A"  `shouldBe` Right [ECInstrW (CInstr False emptyDest (Comp True True False False False False) emptyJump)]

      describe "a -> 0" $ do
        it "M=0"    $ parseW "M=0"    `shouldBe` Right [ECInstrW (CInstr False (Dest False False True) (Comp True   False True  False True  False) emptyJump)]
        it "M=1"    $ parseW "M=1"    `shouldBe` Right [ECInstrW (CInstr False (Dest False False True) (Comp True   True  True  True  True  True) emptyJump)]
        it "M=-1"   $ parseW "M=-1"   `shouldBe` Right [ECInstrW (CInstr False (Dest False False True) (Comp True   True  True  False True  False) emptyJump)]
        it "M=D"    $ parseW "M=D"    `shouldBe` Right [ECInstrW (CInstr False (Dest False False True) (Comp False  False True  True  False False) emptyJump)]
        it "M=A"    $ parseW "M=A"    `shouldBe` Right [ECInstrW (CInstr False (Dest False False True) (Comp True   True  False False False False) emptyJump)]
        it "M=!D"   $ parseW "M=!D"   `shouldBe` Right [ECInstrW (CInstr False (Dest False False True) (Comp False  False True  True  False True) emptyJump)]
        it "M=!A"   $ parseW "M=!A"   `shouldBe` Right [ECInstrW (CInstr False (Dest False False True) (Comp True   True  False False True  True) emptyJump)]
        it "M=-D"   $ parseW "M=-D"   `shouldBe` Right [ECInstrW (CInstr False (Dest False False True) (Comp False  False True  True  True  True) emptyJump)]
        it "M=-A"   $ parseW "M=-A"   `shouldBe` Right [ECInstrW (CInstr False (Dest False False True) (Comp True   True  False False True  True) emptyJump)]
        it "M=D+1"  $ parseW "M=D+1"  `shouldBe` Right [ECInstrW (CInstr False (Dest False False True) (Comp False  True  True  True  True  True) emptyJump)]
        it "M=A+1"  $ parseW "M=A+1"  `shouldBe` Right [ECInstrW (CInstr False (Dest False False True) (Comp True   True  False True  True  True) emptyJump)]
        it "M=D-1"  $ parseW "M=D-1"  `shouldBe` Right [ECInstrW (CInstr False (Dest False False True) (Comp False  False True  True  True  False) emptyJump)]
        it "M=A-1"  $ parseW "M=A-1"  `shouldBe` Right [ECInstrW (CInstr False (Dest False False True) (Comp True   True  False False True  False) emptyJump)]
        it "M=D+A"  $ parseW "M=D+A"  `shouldBe` Right [ECInstrW (CInstr False (Dest False False True) (Comp False  False False False True  False) emptyJump)]
        it "M=D-A"  $ parseW "M=D-A"  `shouldBe` Right [ECInstrW (CInstr False (Dest False False True) (Comp False  True  False False True  True) emptyJump)]
        it "M=A-D"  $ parseW "M=A-D"  `shouldBe` Right [ECInstrW (CInstr False (Dest False False True) (Comp False  False False True  True  True) emptyJump)]
        it "M=D&A"  $ parseW "M=D&A"  `shouldBe` Right [ECInstrW (CInstr False (Dest False False True) (Comp False  False False False False False) emptyJump)]
        it "M=D|A"  $ parseW "M=D|A"  `shouldBe` Right [ECInstrW (CInstr False (Dest False False True) (Comp False  True  False True  False True) emptyJump)]

      describe "a -> 1" $ do
        it "M=M"    $ parseW "M=M"    `shouldBe` Right [ECInstrW (CInstr True (Dest False False True) (Comp True   True  False False False False) emptyJump)]
        it "M=!M"   $ parseW "M=!M"   `shouldBe` Right [ECInstrW (CInstr True (Dest False False True) (Comp True   True  False False False  True) emptyJump)]
        it "M=-M"   $ parseW "M=-M"   `shouldBe` Right [ECInstrW (CInstr True (Dest False False True) (Comp True   True  False False True  True) emptyJump)]
        it "M=M+1"  $ parseW "M=M+1"  `shouldBe` Right [ECInstrW (CInstr True (Dest False False True) (Comp True   True  False True  True  True) emptyJump)]
        it "M=M-1"  $ parseW "M=M-1"  `shouldBe` Right [ECInstrW (CInstr True (Dest False False True) (Comp True   True  False False True  False) emptyJump)]
        it "M=D+M"  $ parseW "M=D+M"  `shouldBe` Right [ECInstrW (CInstr True (Dest False False True) (Comp False  False False False True  False) emptyJump)]
        it "M=D-M"  $ parseW "M=D-M"  `shouldBe` Right [ECInstrW (CInstr True (Dest False False True) (Comp False  True  False False True  True) emptyJump)]
        it "M=M-D"  $ parseW "M=M-D"  `shouldBe` Right [ECInstrW (CInstr True (Dest False False True) (Comp False  False False True  True  True) emptyJump)]
        it "M=D&M"  $ parseW "M=D&M"  `shouldBe` Right [ECInstrW (CInstr True (Dest False False True) (Comp False  False False False False False) emptyJump)]
        it "M=D|M"  $ parseW "M=D|M"  `shouldBe` Right [ECInstrW (CInstr True (Dest False False True) (Comp False  True  False True  False True) emptyJump)]

  describe "Label" $ do
    it "(Label)"  $ parseW "(LABEL)" `shouldBe` Right [ELabelW "LABEL"]
    it "(M)"      $ parseW "(M)"     `shouldBe` Right [ELabelW "M"]
    it "(D)"      $ parseW "(D)"     `shouldBe` Right [ELabelW "D"]
    it "(DM)"     $ parseW "(DM)"    `shouldBe` Right [ELabelW "DM"]
    it "(A)"      $ parseW "(A)"     `shouldBe` Right [ELabelW "A"]
    it "(AM)"     $ parseW "(AM)"    `shouldBe` Right [ELabelW "AM"]
    it "(AD)"     $ parseW "(AD)"    `shouldBe` Right [ELabelW "AD"]
    it "(ADM)"    $ parseW "(ADM)"   `shouldBe` Right [ELabelW "ADM"]


data ExpW
  = EAInstrW AInstr
  | ECInstrW CInstr
  | ELabelW ByteString
  deriving (Eq, Show)

unRange :: Exp Range -> ExpW
unRange (EAInstr _ a) = EAInstrW a
unRange (ECInstr _ c) = ECInstrW c
unRange (ELabel _ l)  = ELabelW l

parseW :: ByteString -> Either String [ExpW]
parseW s = case parse s of
  Left e   -> Left e
  Right ts -> Right $ map unRange ts
