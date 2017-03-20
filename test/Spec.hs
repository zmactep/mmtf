{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

import           Bio.MMTF.Decode
import           Bio.MMTF.Type
import qualified Data.ByteString.Lazy as B
import           Data.MessagePack
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = hspec $
  describe "Hello World" $
    it "should work" $ do
      contents <- B.readFile "resource/1FSD.mmtf"
      m <- (unpack contents) :: IO MMTF
      print m
      1 `shouldBe` 1
