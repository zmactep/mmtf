{-# LANGUAGE OverloadedStrings #-}

import           Bio.MMTF
import qualified Data.ByteString.Lazy as B
import           Test.Hspec

main :: IO ()
main = hspec $
  describe "MMTF" $
    it "should parse 1FSD" $ do
      contents <- B.readFile "resource/1FSD.mmtf"
      m <- decode contents
      (structureId . structure) m `shouldBe` Just "1FSD"
      (numModels . structure) m `shouldBe` 41