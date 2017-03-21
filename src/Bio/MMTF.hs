module Bio.MMTF
  ( module T
  , decode
  ) where

import           Bio.MMTF.MessagePack ()
import           Bio.MMTF.Type        as T

import           Data.ByteString.Lazy (ByteString)
import           Data.MessagePack     (unpack)

-- |Decodes a 'ByteString' to 'MMTF'
decode :: Monad m => ByteString -> m MMTF
decode = unpack
