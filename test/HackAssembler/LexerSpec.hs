module HackAssembler.LexerSpec (spec) where

import           Data.ByteString.Lazy.Char8 (ByteString)
import           HackAssembler.Lexer        (RangedToken (..), Token (..), scan)
import           Test.Hspec                 (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Comment" $ do
    it "line"   $ scanW "// a comment"    `shouldBe` Right [EOF]
    it "inline" $ scanW "@2 // a comment" `shouldBe` Right [Symbol "@", Integer 2, EOF]

  describe "A-Instruction" $ do
    it "constant" $ scanW "@2"      `shouldBe` Right [Symbol "@", Integer 2, EOF]
    it "symbol"   $ scanW "@a_.:$"  `shouldBe` Right [Symbol "@", Identifier "a_.:$", EOF]

  describe "C-Instruction" $ do
    it "AMD=D+1;JMP"  $ scanW "AMD=D+1;JMP" `shouldBe` Right [Identifier "AMD", Symbol "=", Identifier "D", Symbol "+", Integer 1, Symbol ";", Identifier "JMP", EOF]
    it "AMD=D-1"      $ scanW "AMD=D-1"     `shouldBe` Right [Identifier "AMD", Symbol "=", Identifier "D", Symbol "-", Integer 1, EOF]
    it "AMD=!D"       $ scanW "AMD=!D"      `shouldBe` Right [Identifier "AMD", Symbol "=", Symbol "!", Identifier "D", EOF]
    it "AMD=D|M"      $ scanW "AMD=D|M"     `shouldBe` Right [Identifier "AMD", Symbol "=", Identifier "D", Symbol "|", Identifier "M", EOF]
    it "AMD=D&M"      $ scanW "AMD=D&M"     `shouldBe` Right [Identifier "AMD", Symbol "=", Identifier "D", Symbol "&", Identifier "M", EOF]

  it "Label" $ scanW "(LABEL)" `shouldBe` Right [Symbol "(", Identifier "LABEL", Symbol ")", EOF]

scanW :: ByteString -> Either String [Token]
scanW s = case scan s of
                Left e    -> Left e
                Right rts -> Right $ map rtToken rts
