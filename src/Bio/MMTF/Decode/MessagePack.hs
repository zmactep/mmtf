module Bio.MMTF.Decode.MessagePack where

import           Data.ByteString.Lazy  (ByteString, fromStrict)
import           Data.Map.Strict       (Map, fromList)
import qualified Data.Map.Strict       as M (lookup)
import           Data.MessagePack
import           Data.Text             (Text)
import qualified Data.Text             as T (unpack)

transformObjectMap :: Monad m => Object -> m (Map Text Object)
transformObjectMap (ObjectMap kv) = let mkPair :: Monad m => (Object, Object) -> m (Text, Object)
                                        mkPair (ObjectStr txt, v) = pure (txt, v)
                                        mkPair _ = fail "Non-string key"
                                    in  fromList <$> traverse mkPair kv
transformObjectMap _ = fail "Wrong MessagePack MMTF format"

atP :: Monad m => Map Text Object -> Text -> (Object -> m a) -> m a
atP m k conv =
  case M.lookup k m of
    Just x  -> conv x
    Nothing -> fail $ "Required field '" ++ uk ++ "' was not found"
  where uk = T.unpack k

atPM :: Monad m => Map Text Object -> Text -> (Object -> m a) -> m (Maybe a)
atPM m k conv = traverse conv $ M.lookup k m

atPMD :: Monad m => Map Text Object -> Text -> (Object -> m a) -> a -> m a
atPMD m k conv def = do x <- atPM m k conv
                        case x of
                          Just r  -> pure r
                          Nothing -> pure def
                       
asStr :: Monad m => Object -> m Text
asStr (ObjectStr s) = pure s
asStr _             = fail "Not a string data"

asChar :: Monad m => Object -> m Char
asChar = (head . T.unpack <$>) . asStr

asInt :: (Monad m, Integral a) => Object -> m a
asInt (ObjectInt i)  = pure (fromIntegral i)
asInt (ObjectWord w) = pure (fromIntegral w)
asInt _              = fail "Not an int data"

asFloat :: Monad m => Object -> m Float
asFloat (ObjectFloat f) = pure f
asFloat _               = fail "Not a float data"

asIntList :: (Monad m, Integral a) => Object -> m [a]
asIntList (ObjectArray l) = traverse asInt l
asIntList _               = fail "Not an array of ints data"

asStrList :: Monad m => Object -> m [Text]
asStrList (ObjectArray l) = traverse asStr l
asStrList _               = fail "Not an array of string data"

asFloatList :: Monad m => Object -> m [Float]
asFloatList (ObjectArray l) = traverse asFloat l
asFloatList _               = fail "Not an array of float data"

asObjectList :: Monad m => Object -> m [Object]
asObjectList (ObjectArray l) = pure l
asObjectList _               = fail "Not an array data"

asBinary :: Monad m => Object -> m ByteString
asBinary (ObjectBin bs) = pure (fromStrict bs)
asBinary _              = fail "Not a binary data"
