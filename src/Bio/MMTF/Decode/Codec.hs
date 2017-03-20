module Bio.MMTF.Decode.Codec where

import           Data.Binary          (Binary, decode)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B (length, null, splitAt, unpack)
import           Data.Char            (chr)
import           Data.Int             (Int16, Int32, Int8)
import           Data.List            (mapAccumL)
import           Data.Text            (Text)
import qualified Data.Text            as T (pack)

import           Bio.MMTF.Type

codecCommon :: Binary a => (ByteString -> a) -> Int -> ByteString -> [a]
codecCommon  f th bs | B.null bs         = []
                     | B.length bs < ith = error "Wrong number of bytes in bytestring"
                     | otherwise         = let (start, rest) = B.splitAt ith bs
                                           in f start : codecCommon f th rest
  where ith = fromIntegral th

data BinaryData = BD { binaryCodec  :: Int32
                     , binaryLength :: Int32
                     , binaryParam  :: Int32
                     , binaryData   :: ByteString
                     }

-- |Parse useless header for binary data
parseBinary :: ByteString -> BinaryData
parseBinary bs = let (cdc, rest1) = B.splitAt 4 bs
                     (lnh, rest2) = B.splitAt 4 rest1
                     (prm, rest)  = B.splitAt 4 rest2
                 in  BD (decode cdc) (decode lnh) (decode prm) rest

-- |Interpret bytes as array of 32-bit floating-point numbers.
codec1 :: BinaryData -> [Float]
codec1 = codecCommon decode 4 . binaryData

-- |Interpret bytes as array of 8-bit signed integers.
codec2 :: BinaryData -> [Int8]
codec2 = codecCommon decode 1 . binaryData

-- |Interpret bytes as array of 16-bit signed integers.
codec3 :: BinaryData -> [Int16]
codec3 = codecCommon decode 2 . binaryData

-- |Interpret bytes as array of 32-bit signed integers.
codec4 :: BinaryData -> [Int32]
codec4 = codecCommon decode 4 . binaryData

-- |Interpret bytes as array of 8-bit unsigned integers, then iteratively
-- consume length many bytes to form a string array.
codec5 :: BinaryData -> [Text]
codec5 bd = codecCommon decodeBytes (fromIntegral $ binaryParam bd) (binaryData bd)
  where decodeBytes :: ByteString -> Text
        decodeBytes bs = T.pack $ chr <$> filter (/=0) (fromIntegral <$> B.unpack bs)

-- |Interpret bytes as array of 32-bit signed integers, then run-length
-- decode into array of characters.
codec6 :: BinaryData -> [Char]
codec6 = map (chr . fromIntegral) . codec7

-- |Interpret bytes as array of 32-bit signed integers, then run-length
-- decode into array of 32-bit signed integers.
codec7 :: BinaryData -> [Int32]
codec7 = runLengthDec . codec4

-- |Interpret bytes as array of 32-bit signed integers, then run-length
-- decode into array of 32-bit signed integers, then delta decode into
-- array of 32-bit signed integers.
codec8 :: BinaryData -> [Int32]
codec8 = deltaDec . codec7

-- |Interpret bytes as array of 32-bit signed integers, then run-length
-- decode into array of 32-bit signed integers, then integer decode into
-- array of 32-bit floating-point numbers using the divisor parameter.
codec9 :: BinaryData -> [Float]
codec9 bd = integerDec (binaryParam bd) $ codec7 bd

-- |Interpret bytes as array of 16-bit signed integers, then unpack into
-- array of 32-bit integers, then delta decode into array of 32-bit
-- integers, then integer decode into array of 32-bit floating-point
-- numbers using the divisor parameter.
codec10 :: BinaryData -> [Float]
codec10 bd = integerDec (binaryParam bd) $ map fromIntegral $ deltaDec $ codec3 bd

-- |Interpret bytes as array of 16-bit signed integers, then integer
-- decode into array of 32-bit floating-point numbers using the divisor parameter.
codec11 :: BinaryData -> [Float]
codec11 bd = integerDec (binaryParam bd) $ map fromIntegral $ codec3 bd

-- |Interpret bytes as array of 16-bit signed integers, then unpack into
-- array of 32-bit signed integers, then integer decode into array
-- of 32-bit floating-point numbers using the divisor parameter.
codec12 :: BinaryData -> [Float]
codec12 bd = integerDec (binaryParam bd) $ recIndexDec $ codec3 bd

-- |Interpret array of bytes as array of 8-bit signed integers, then
-- unpack into array of 32-bit signed integers, then integer decode into
-- array of 32-bit floating-point numbers using the divisor parameter.
codec13 :: BinaryData -> [Float]
codec13 bd = integerDec (binaryParam bd) $ recIndexDec $ codec2 bd

-- |Interpret bytes as array of 16-bit signed integers, then unpack
-- into array of 32-bit signed integers.
codec14 :: BinaryData -> [Int32]
codec14 bd = recIndexDec $ codec3 bd

-- |Interpret bytes as array of 8-bit signed integers, then unpack
-- into array of 32-bit signed integers.
codec15 :: BinaryData -> [Int32]
codec15 bd = recIndexDec $ codec2 bd

-- Decodings

runLengthDec :: Integral a => [a] -> [a]
runLengthDec [] = []
runLengthDec [_] = error "List must have even length for run-length encoding"
runLengthDec (x:l:xs) = (replicate (fromIntegral l) x) ++ runLengthDec xs

deltaDec :: Num a => [a] -> [a]
deltaDec = snd . mapAccumL (\x y -> (x+y,x+y)) 0

recIndexDec :: (Integral a, Bounded a, Eq a) => [a] -> [Int32]
recIndexDec [] = []
recIndexDec xs = recIndexDecAcc 0 xs
  where recIndexDecAcc :: (Integral a, Bounded a) => Int32 -> [a] -> [Int32]
        recIndexDecAcc acc []     | acc /= 0  = [acc]
                                  | otherwise = []
        recIndexDecAcc acc (x:ys) | x > minBound && x < maxBound = fromIntegral x + acc : recIndexDecAcc 0 ys
                                  | otherwise                    = recIndexDecAcc (fromIntegral x + acc) ys

integerDec :: Integral a => a -> [a] -> [Float]
integerDec divisor = map (\x -> fromIntegral x / fromIntegral divisor)

ssDec :: Int8 -> SecondaryStructure
ssDec n | n == 0    = PiHelix
        | n == 1    = Bend
        | n == 2    = AlphaHelix
        | n == 3    = Extended
        | n == 4    = ThreeTenHelix
        | n == 5    = Bridge
        | n == 6    = Turn
        | n == 7    = Coil
        | otherwise = Undefined

ucDec :: Monad m => [Float] -> m UnitCell
ucDec [a,b,c,d,e,f] = pure $ UnitCell a b c d e f
ucDec _  = fail "Wrong list format for unit cell"
