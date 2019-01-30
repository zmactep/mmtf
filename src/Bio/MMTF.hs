module Bio.MMTF
  ( module Bio.MMTF.Type
  , decode
  , fetch
  ) where

import           Bio.MMTF.MessagePack ()
import           Bio.MMTF.Type

import           Data.ByteString.Lazy   (ByteString)
import           Data.MessagePack       (unpack)
import           Data.Monoid            ((<>))
import           Data.String            (IsString (..))
import           Control.Monad.IO.Class (MonadIO)
import           Network.HTTP.Simple    (httpLBS, getResponseBody)

-- | Decodes a 'ByteString' to 'MMTF'
--
decode :: Monad m => ByteString -> m MMTF
decode = unpack

-- | Fetches MMTF structure from RSCB
fetch :: MonadIO m => String -> m MMTF
fetch pdbid = do let url = fromString $ "https://mmtf.rcsb.org/v1.0/full/" <> pdbid
                 resp <- httpLBS url
                 decode (getResponseBody resp) 
