module HackAssembler.EvalSpec (spec) where

import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import           HackAssembler.AST          (AInstr (..), CInstr (..),
                                             Comp (Comp), Dest (Dest), Exp (..),
                                             Jump (Jump), emptyDest, emptyJump)
import           HackAssembler.Eval         (eval)
import           HackAssembler.Lexer        (Range (..))
import           HackAssembler.Parser       (parse)
import           Test.Hspec                 (Spec, it, shouldBe)

spec :: Spec
spec = do
  it "Rect.asm" $ do
    file <- BS.readFile "test/files/rect/Rect.asm"
    evalW file `shouldBe` Right
      [ EAInstrW (Const 0)                                                                                    -- @0
      , ECInstrW (CInstr True (Dest False True False) (Comp True True False False False False) emptyJump)     -- D=M
      , EAInstrW (Const 23)                                                                                   -- @INFINITE_LOOP
      , ECInstrW (CInstr False emptyDest (Comp False False True True False False) (Jump True True False))     -- D;JLE
      , EAInstrW (Const 16)                                                                                   -- @counter
      , ECInstrW (CInstr False (Dest False False True) (Comp False False True True False False) emptyJump)    -- M=D
      , EAInstrW (Const 16384)                                                                                -- @SCREEN
      , ECInstrW (CInstr False (Dest False True False) (Comp True True False False False False) emptyJump)    -- D=A
      , EAInstrW (Const 17)                                                                                   -- @address
      , ECInstrW (CInstr False (Dest False False True) (Comp False False True True False False) emptyJump)    -- M=D
      , EAInstrW (Const 17)                                                                                   -- @address
      , ECInstrW (CInstr True (Dest True False False) (Comp True True False False False False) emptyJump)     -- A=M
      , ECInstrW (CInstr False (Dest False False True) (Comp True True True False True False) emptyJump)      -- M=D
      , EAInstrW (Const 17)                                                                                   -- @address
      , ECInstrW (CInstr True (Dest False True False) (Comp True True False False False False) emptyJump)     -- D=M
      , EAInstrW (Const 32)                                                                                   -- @32
      , ECInstrW (CInstr False (Dest False True False) (Comp False False False False True False) emptyJump)   -- D=D+A
      , EAInstrW (Const 17)                                                                                   -- @address
      , ECInstrW (CInstr False (Dest False False True) (Comp False False True True False False) emptyJump)    -- M=D
      , EAInstrW (Const 16)                                                                                   -- @counter
      , ECInstrW (CInstr True (Dest False True True) (Comp True True False False True False) emptyJump)       -- MD=M-1
      , EAInstrW (Const 10)                                                                                   -- @LOOP
      , ECInstrW (CInstr False emptyDest (Comp False False True True False False) (Jump False False True))    -- D;JGT
      , EAInstrW (Const 23)                                                                                   -- @INFINITE_LOOP
      , ECInstrW (CInstr False emptyDest (Comp True False True False True False) (Jump True True True))       -- 0;JMP
      ]



data ExpW
  = EAInstrW AInstr
  | ECInstrW CInstr
  | ELabelW ByteString
  deriving (Eq, Show)

unRange :: Exp Range -> ExpW
unRange (EAInstr _ a) = EAInstrW a
unRange (ECInstr _ c) = ECInstrW c
unRange (ELabel _ l)  = ELabelW l

evalW :: ByteString -> Either String [ExpW]
evalW s = case eval s of
  Left e   -> Left e
  Right ts -> Right $ map unRange ts
