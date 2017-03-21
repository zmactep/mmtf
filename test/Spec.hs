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
      let sid = (structureId . structure) m
      sid `shouldBe` Just "1FSD"
