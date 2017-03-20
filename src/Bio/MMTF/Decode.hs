{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module Bio.MMTF.Decode where

import           Bio.MMTF.Decode.Codec
import           Bio.MMTF.Type

import           Data.ByteString.Lazy  (ByteString, fromStrict)
import           Data.Map.Strict       (Map, fromList)
import qualified Data.Map.Strict       as M (lookup)
import           Data.MessagePack
import           Data.Text             (Text)
import qualified Data.Text             as T (unpack)

instance MessagePack MMTF where
  toObject = undefined
  fromObject obj = do mp <- transformObjectMap obj
                      f <- formatData mp
                      s <- structureData mp
                      m <- modelData mp
                      c <- chainData mp
                      g <- groupData mp
                      a <- atomData mp
                      pure $ MMTF f s m c g a

transformObjectMap :: Monad m => Object -> m (Map Text Object)
transformObjectMap (ObjectMap kv) = let mkPair :: Monad m => (Object, Object) -> m (Text, Object)
                                        mkPair ((ObjectStr txt), v) = pure (txt, v)
                                        mkPair _ = fail "Non-string key"
                                    in  fromList <$> sequence (map mkPair kv)
transformObjectMap _ = fail "Wrong MessagePack MMTF format"

-- |Parses format data from ObjectMap
formatData :: Monad m => Map Text Object -> m FormatData
formatData mp = do v <- atP mp "mmtfVersion" asStr
                   p <- atP mp "mmtfProducer" asStr
                   pure $ FormatData v p

-- |Parses model data from ObjectMap
modelData :: Monad m => Map Text Object -> m ModelData
modelData mp = atP mp "chainsPerModel" asIntList >>= return . ModelData

-- |Parses chain data from ObjectMap
chainData :: Monad m => Map Text Object -> m ChainData
chainData mp = do gpc <- atP mp "groupsPerChain" asIntList
                  cil <- codec5 . parseBinary <$> atP mp "chainIdList" asBinary
                  cnl <- (codec5 . parseBinary <$>) <$> atPM mp "chainNameList" asBinary
                  pure $ ChainData gpc cil cnl

-- |Parses atom data from ObjectMap
atomData :: Monad m => Map Text Object -> m AtomData
atomData mp = do ail' <- (codec8 . parseBinary <$>) <$> atPM mp "atomIdList" asBinary
                 all' <- (codec6 . parseBinary <$>) <$> atPM mp "altLocList" asBinary
                 bfl' <- (codec10 . parseBinary <$>) <$> atPM mp "bFactorList" asBinary
                 xcl' <- codec10 . parseBinary <$> atP mp "xCoordList" asBinary
                 ycl' <- codec10 . parseBinary <$> atP mp "yCoordList" asBinary
                 zcl' <- codec10 . parseBinary <$> atP mp "zCoordList" asBinary
                 ol' <-  (codec9 . parseBinary <$>) <$> atPM mp "occupancyList" asBinary
                 pure $ AtomData ail' all' bfl' xcl' ycl' zcl' ol'

-- |Parses group data from ObjectMap
groupData :: Monad m => Map Text Object -> m GroupData
groupData mp = do gl' <- atP mp "groupList" asObjectList >>= sequence . map (\x -> transformObjectMap x >>= groupType)
                  gtl' <- codec4 . parseBinary <$> atP mp "groupTypeList" asBinary
                  gil' <- codec8 . parseBinary <$> atP mp "groupIdList" asBinary
                  ssl' <- (map ssDec . codec2 . parseBinary <$>) <$> atPM mp "secStructList" asBinary
                  icl' <- (codec6 . parseBinary <$>) <$> atPM mp "insCodeList" asBinary
                  sil' <- (codec8 . parseBinary <$>) <$> atPM mp "sequenceIndexList" asBinary
                  pure $ GroupData gl' gtl' gil' ssl' icl' sil'

-- |Parses group type from ObjectMap
groupType :: Monad m => Map Text Object -> m GroupType
groupType mp = do fcl' <- atP mp "formalChargeList" asIntList
                  anl' <- atP mp "atomNameList" asStrList
                  el'  <- atP mp "elementList" asStrList
                  bal' <- atP mp "bondAtomList" asIntList
                  bol' <- atP mp "bondOrderList" asIntList
                  gn'  <- atP mp "groupName" asStr
                  slc' <- atP mp "singleLetterCode" asChar
                  cct' <- atP mp "chemCompType" asStr
                  pure $ GroupType fcl' anl' el' bal' bol' gn' slc' cct'

structureData :: Monad m => Map Text Object -> m StructureData
structureData mp = do ttl' <- atPM mp "title" asStr
                      sid' <- atPM mp "strucutreId" asStr
                      dd'  <- atPM mp "depositionDate" asStr
                      rd'  <- atPM mp "releaseDate" asStr
                      nb'  <- atP mp "numBonds" asInt
                      na'  <- atP mp "numAtoms" asInt
                      ng'  <- atP mp "numGroups" asInt
                      nc'  <- atP mp "numChains" asInt
                      nm'  <- atP mp "numModels" asInt
                      sg'  <- atPM mp "spaceGroup" asStr
                      uc'  <- (>>= ucDec) <$> atPM mp "unitCell" asFloatList
                      nol' <- ((>>= asFloatList) <$>) <$> atPM mp "ncsOperatorList" asObjectList
                      bal' <- atPM mp "bioAssemblyList" asObjectList
                      el'  <- atPM mp "entityList" asObjectList
                      res' <- atPM mp "resolution" asFloat
                      rf'  <- atPM mp "rFree" asFloat
                      rw'  <- atPM mp "rWork" asFloat
                      em'  <- atPM mp "experimentalMethods" asStrList
                      bal' <- (codec4 . parseBinary <$>) <$> atPM mp "bondAtomList" asBinary
                      bol' <- (codec2 . parseBinary <$>) <$> atPM mp "bondOrderList" asBinary
                      pure $ StructureData ttl' sid' dd' rd' nb' na' ng' nc' nm' sg' uc' nol'
                                           (Just ()) (Just ()) res' rf' rw' em' bal' bol'

-- Helper functions

atP :: Monad m => Map Text Object -> Text -> (Object -> m a) -> m a
atP m k conv =
  case M.lookup k m of
    Just x  -> conv x
    Nothing -> fail $ "Required field '" ++ uk ++ "' was not found"
  where uk = T.unpack k

atPM :: Monad m => Map Text Object -> Text -> (Object -> m a) -> m (Maybe a)
atPM m k conv = traverse conv $ M.lookup k m

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
asIntList (ObjectArray l) = sequence $ map asInt l
asIntList _               = fail "Not an array of ints data"

asStrList :: Monad m => Object -> m [Text]
asStrList (ObjectArray l) = sequence $ map asStr l
asStrList _               = fail "Not an array of string data"

asFloatList :: Monad m => Object -> m [Float]
asFloatList (ObjectArray l) = sequence $ map asFloat l
asFloatList _               = fail "Not an array of float data"

asObjectList :: Monad m => Object -> m [Object]
asObjectList (ObjectArray l) = pure l
asObjectList _               = fail "Not an array data"

asBinary :: Monad m => Object -> m ByteString
asBinary (ObjectBin bs) = pure (fromStrict bs)
asBinary _              = fail "Not a binary data"
