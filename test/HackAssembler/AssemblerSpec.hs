module HackAssembler.AssemblerSpec (spec) where

import           Control.Monad              (when)
import           Data.ByteString.Lazy.Char8 as BS
import           Data.Either                (fromRight, isRight)
import           HackAssembler.Assembler    (assemble)
import           Test.Hspec                 (Spec, it, shouldBe)

spec :: Spec
spec = do
  it "Add.asm" $ do
    asm <- BS.readFile "test/files/add/Add.asm"
    hack <- BS.readFile "test/files/add/Add.hack"
    let hack' = assemble asm
    writeTest "test/files/add/Add.test.hack" hack'
    hack' `shouldBe` Right hack

  it "MaxL.asm" $ do
    asm <- BS.readFile "test/files/max/MaxL.asm"
    hack <- BS.readFile "test/files/max/MaxL.hack"
    let hack' = assemble asm
    writeTest "test/files/max/MaxL.test.hack" hack'
    hack' `shouldBe` Right hack

  it "Max.asm" $ do
    asm <- BS.readFile "test/files/max/Max.asm"
    hack <- BS.readFile "test/files/max/Max.hack"
    let hack' = assemble asm
    writeTest "test/files/max/Max.test.hack" hack'
    hack' `shouldBe` Right hack

  it "RectL.asm" $ do
    asm <- BS.readFile "test/files/rect/RectL.asm"
    hack <- BS.readFile "test/files/rect/RectL.hack"
    let hack' = assemble asm
    writeTest "test/files/rect/RectL.test.hack" hack'
    hack' `shouldBe` Right hack

  it "Rect.asm" $ do
    asm <- BS.readFile "test/files/rect/Rect.asm"
    hack <- BS.readFile "test/files/rect/Rect.hack"
    let hack' = assemble asm
    writeTest "test/files/rect/Rect.test.hack" hack'
    hack' `shouldBe` Right hack

  it "PongL.asm" $ do
    asm <- BS.readFile "test/files/pong/PongL.asm"
    hack <- BS.readFile "test/files/pong/PongL.hack"
    let hack' = assemble asm
    writeTest "test/files/pong/PongL.test.hack" hack'
    hack' `shouldBe` Right hack

  it "Pong.asm" $ do
    asm <- BS.readFile "test/files/pong/Pong.asm"
    hack <- BS.readFile "test/files/pong/Pong.hack"
    let hack' = assemble asm
    writeTest "test/files/pong/Pong.test.hack" hack'
    hack' `shouldBe` Right hack

writeTest :: FilePath -> Either String ByteString -> IO ()
writeTest _ (Left _)     = return ()
writeTest path (Right p) = BS.writeFile path p
