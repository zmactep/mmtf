module Bio.MMTF
  ( module Bio.MMTF.Type
  , decode
  ) where

import           Bio.MMTF.MessagePack ()
import           Bio.MMTF.Type

import           Data.ByteString.Lazy (ByteString)
import           Data.MessagePack     (unpack)

-- |Decodes a 'ByteString' to 'MMTF'
decode :: Monad m => ByteString -> m MMTF
decode = unpack
