module Bio.MMTF.Decode.MessagePack where

import           Data.ByteString.Lazy  (ByteString, fromStrict)
import           Data.Map.Strict       (Map, fromList)
import qualified Data.Map.Strict       as M (lookup)
import           Data.MessagePack
import           Data.Monoid           ((<>))
import           Data.Text             (Text)
import qualified Data.Text             as T (unpack)

transformObjectMap :: Monad m => Object -> m (Map Text Object)
transformObjectMap (ObjectMap kv) = let mkPair :: Monad m => (Object, Object) -> m (Text, Object)
                                        mkPair (ObjectStr txt, v) = pure (txt, v)
                                        mkPair _ = fail "Non-string key"
                                    in  fromList <$> traverse mkPair kv
transformObjectMap _ = fail "Wrong MessagePack MMTF format"

atP :: Monad m => Map Text Object -> Text -> (Text -> Object -> m a) -> m a
atP m k conv =
  case M.lookup k m of
    Just x  -> conv k x
    Nothing -> fail $ "Required field '" ++ uk ++ "' was not found"
  where uk = T.unpack k

atPM :: Monad m => Map Text Object -> Text -> (Text -> Object -> m a) -> m (Maybe a)
atPM m k conv = traverse (conv k) $ M.lookup k m

atPMD :: Monad m => Map Text Object -> Text -> (Text -> Object -> m a) -> a -> m a
atPMD m k conv def = do x <- atPM m k conv
                        case x of
                          Just r  -> pure r
                          Nothing -> pure def
                       
asStr :: Monad m => Text -> Object -> m Text
asStr _ (ObjectStr s) = pure s
asStr m _             = fail $ T.unpack m <> ": not a string data"

asChar :: Monad m => Text -> Object -> m Char
asChar m = (head . T.unpack <$>) . asStr m

asInt :: (Monad m, Integral a) => Text -> Object -> m a
asInt _ (ObjectInt i)  = pure (fromIntegral i)
asInt _ (ObjectWord w) = pure (fromIntegral w)
asInt m _              = fail $ T.unpack m <> ": not an int data"

asFloat :: Monad m => Text -> Object -> m Float
asFloat _ (ObjectFloat  f) = pure f
asFloat _ (ObjectDouble f) = pure (realToFrac f)
asFloat m _                = fail $ T.unpack m <> ": not a float data"

asIntList :: (Monad m, Integral a) => Text -> Object -> m [a]
asIntList m (ObjectArray l) = traverse (asInt m) l
asIntList m _               = fail $ T.unpack m <> ": not an array of ints data"

asStrList :: Monad m => Text -> Object -> m [Text]
asStrList m (ObjectArray l) = traverse (asStr m) l
asStrList m _               = fail $ T.unpack m <> ": not an array of string data"

asFloatList :: Monad m => Text -> Object -> m [Float]
asFloatList m (ObjectArray l) = traverse (asFloat m) l
asFloatList m _               = fail $ T.unpack m <> ": not an array of float data"

asObjectList :: Monad m => Text -> Object -> m [Object]
asObjectList _ (ObjectArray l) = pure l
asObjectList m _               = fail $ T.unpack m <> ": not an array data"

asBinary :: Monad m => Text -> Object -> m ByteString
asBinary _ (ObjectBin bs) = pure (fromStrict bs)
asBinary m _              = fail $ T.unpack m <> ": not a binary data"
