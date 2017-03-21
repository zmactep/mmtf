module Bio.MMTF
  ( module T
  , decode
  ) where

import Bio.MMTF.Type as T
import Bio.MMTF.MessagePack ()

import Data.MessagePack (unpack)
import Data.ByteString.Lazy (ByteString)

-- |Decodes a 'ByteString' to 'MMTF'
decode :: Monad m => ByteString -> m MMTF
decode = unpack
