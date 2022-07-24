module HackAssembler.ParserSpec (spec) where

import           Data.ByteString.Lazy.Char8 (ByteString)
import           HackAssembler.AST          (AInstr (..), CInstr (CInstr),
                                             Exp (..))
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
      it "  M=A"  $ parseW "M=A"    `shouldBe` Right [ECInstrW (CInstr False (Just "M") "A" Nothing)]
      it "  D=A"  $ parseW "D=A"    `shouldBe` Right [ECInstrW (CInstr False (Just "D") "A" Nothing)]
      it " DM=A"  $ parseW "DM=A"   `shouldBe` Right [ECInstrW (CInstr False (Just "DM") "A" Nothing)]
      it "  A=A"  $ parseW "A=A"    `shouldBe` Right [ECInstrW (CInstr False (Just "A") "A" Nothing)]
      it " AM=A"  $ parseW "AM=A"   `shouldBe` Right [ECInstrW (CInstr False (Just "AM") "A" Nothing)]
      it " AD=A"  $ parseW "AD=A"   `shouldBe` Right [ECInstrW (CInstr False (Just "AD") "A" Nothing)]
      it "ADM=A"  $ parseW "ADM=A"  `shouldBe` Right [ECInstrW (CInstr False (Just "ADM") "A" Nothing)]

    describe "jump" $ do
      it "A;JGT"  $ parseW "A;JGT"  `shouldBe` Right [ECInstrW (CInstr False Nothing "A" (Just "JGT"))]
      it "A;JEQ"  $ parseW "A;JEQ"  `shouldBe` Right [ECInstrW (CInstr False Nothing "A" (Just "JEQ"))]
      it "A;JGE"  $ parseW "A;JGE"  `shouldBe` Right [ECInstrW (CInstr False Nothing "A" (Just "JGE"))]
      it "A;JLT"  $ parseW "A;JLT"  `shouldBe` Right [ECInstrW (CInstr False Nothing "A" (Just "JLT"))]
      it "A;JNE"  $ parseW "A;JNE"  `shouldBe` Right [ECInstrW (CInstr False Nothing "A" (Just "JNE"))]
      it "A;JLE"  $ parseW "A;JLE"  `shouldBe` Right [ECInstrW (CInstr False Nothing "A" (Just "JLE"))]
      it "A;JMP"  $ parseW "A;JMP"  `shouldBe` Right [ECInstrW (CInstr False Nothing "A" (Just "JMP"))]

    describe "dest comp jump" $ do
      it "M=A;JGT"  $ parseW "M=A;JGT"  `shouldBe` Right [ECInstrW (CInstr False (Just "M") "A" (Just "JGT"))]

    describe "comp" $ do
      it "M"  $ parseW "M"  `shouldBe` Right [ECInstrW (CInstr True Nothing "M" Nothing)]

      describe "a -> 0" $ do
        it "M=0"    $ parseW "M=0"    `shouldBe` Right [ECInstrW (CInstr False (Just "M") "0" Nothing)]
        it "M=1"    $ parseW "M=1"    `shouldBe` Right [ECInstrW (CInstr False (Just "M") "1" Nothing)]
        it "M=-1"   $ parseW "M=-1"   `shouldBe` Right [ECInstrW (CInstr False (Just "M") "-1" Nothing)]
        it "M=D"    $ parseW "M=D"    `shouldBe` Right [ECInstrW (CInstr False (Just "M") "D" Nothing)]
        it "M=A"    $ parseW "M=A"    `shouldBe` Right [ECInstrW (CInstr False (Just "M") "A" Nothing)]
        it "M=!D"   $ parseW "M=!D"   `shouldBe` Right [ECInstrW (CInstr False (Just "M") "!D" Nothing)]
        it "M=!A"   $ parseW "M=!A"   `shouldBe` Right [ECInstrW (CInstr False (Just "M") "!A" Nothing)]
        it "M=-D"   $ parseW "M=-D"   `shouldBe` Right [ECInstrW (CInstr False (Just "M") "-D" Nothing)]
        it "M=-A"   $ parseW "M=-A"   `shouldBe` Right [ECInstrW (CInstr False (Just "M") "-A" Nothing)]
        it "M=D+1"  $ parseW "M=D+1"  `shouldBe` Right [ECInstrW (CInstr False (Just "M") "D+1" Nothing)]
        it "M=A+1"  $ parseW "M=A+1"  `shouldBe` Right [ECInstrW (CInstr False (Just "M") "A+1" Nothing)]
        it "M=D-1"  $ parseW "M=D-1"  `shouldBe` Right [ECInstrW (CInstr False (Just "M") "D-1" Nothing)]
        it "M=A-1"  $ parseW "M=A-1"  `shouldBe` Right [ECInstrW (CInstr False (Just "M") "A-1" Nothing)]
        it "M=D+A"  $ parseW "M=D+A"  `shouldBe` Right [ECInstrW (CInstr False (Just "M") "D+A" Nothing)]
        it "M=D-A"  $ parseW "M=D-A"  `shouldBe` Right [ECInstrW (CInstr False (Just "M") "D-A" Nothing)]
        it "M=A-D"  $ parseW "M=A-D"  `shouldBe` Right [ECInstrW (CInstr False (Just "M") "A-D" Nothing)]
        it "M=D&A"  $ parseW "M=D&A"  `shouldBe` Right [ECInstrW (CInstr False (Just "M") "D&A" Nothing)]
        it "M=D|A"  $ parseW "M=D|A"  `shouldBe` Right [ECInstrW (CInstr False (Just "M") "D|A" Nothing)]

      describe "a -> 1" $ do
        it "M=M"    $ parseW "M=M"    `shouldBe` Right [ECInstrW (CInstr True (Just "M") "M" Nothing)]
        it "M=!M"   $ parseW "M=!M"   `shouldBe` Right [ECInstrW (CInstr True (Just "M") "!M" Nothing)]
        it "M=-M"   $ parseW "M=-M"   `shouldBe` Right [ECInstrW (CInstr True (Just "M") "-M" Nothing)]
        it "M=M+1"  $ parseW "M=M+1"  `shouldBe` Right [ECInstrW (CInstr True (Just "M") "M+1" Nothing)]
        it "M=M-1"  $ parseW "M=M-1"  `shouldBe` Right [ECInstrW (CInstr True (Just "M") "M-1" Nothing)]
        it "M=D+M"  $ parseW "M=D+M"  `shouldBe` Right [ECInstrW (CInstr True (Just "M") "D+M" Nothing)]
        it "M=D-M"  $ parseW "M=D-M"  `shouldBe` Right [ECInstrW (CInstr True (Just "M") "D-M" Nothing)]
        it "M=M-D"  $ parseW "M=M-D"  `shouldBe` Right [ECInstrW (CInstr True (Just "M") "M-D" Nothing)]
        it "M=D&M"  $ parseW "M=D&M"  `shouldBe` Right [ECInstrW (CInstr True (Just "M") "D&M" Nothing)]
        it "M=D|M"  $ parseW "M=D|M"  `shouldBe` Right [ECInstrW (CInstr True (Just "M") "D|M" Nothing)]

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
