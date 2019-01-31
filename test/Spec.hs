{-# LANGUAGE OverloadedStrings #-}

import           Bio.MMTF
import qualified Data.ByteString.Lazy as B
import           Data.Int             (Int8)
import           Test.Hspec

import Bio.MMTF.Decode.Codec

codecSpec :: Spec
codecSpec =
  describe "MMTF decoding" $ do
    it "unpacks by Run-length encoding" $ do
      let sample = [ 1, 10, 2, 1, 1, 4 ] :: [Int8]
      runLengthDec sample `shouldBe` ([ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1 ] :: [Int8])
    it "unpacks by Delta encoding" $ do
      let sample = [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1 ] :: [Int8]
      deltaDec sample `shouldBe` ([ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 13, 14, 15, 16 ] :: [Int8])
    it "unpacks by Recursive indexing encoding" $ do
      let sample = [ 127, 41, 34, 1, 0, -50, -128, 0, 7, 127, 0, 127, 127, 14 ] :: [Int8]
      recIndexDec sample `shouldBe` [ 168, 34, 1, 0, -50, -128, 7, 127, 268 ]
    it "upacks by Integer encoding" $ do
      let sample = [ 100, 100, 100, 100, 50, 50 ] :: [Int8]
      integerDec 100 sample `shouldBe` [ 1.00, 1.00, 1.00, 1.00, 0.50, 0.50 ]

parserSpec :: Spec
parserSpec =
  describe "MMTF parser" $
  it "should parse 1FSD" $ do
    m <- fetch "1FSD"
    (structureId . structure) m `shouldBe` "1FSD"
    (numModels . structure) m `shouldBe` 41
    (length . bFactorList . atom) m `shouldBe` 20664
    (experimentalMethods . structure) m `shouldBe` ["SOLUTION NMR"]
    (head . xCoordList . atom) m `shouldBe` (-12.847)
    (last . xCoordList . atom) m `shouldBe` 5.672

main :: IO ()
main = hspec $ do
         codecSpec
         parserSpec