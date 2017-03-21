{-# LANGUAGE OverloadedStrings #-}

module Bio.MMTF.Decode where

import           Bio.MMTF.Decode.Codec
import           Bio.MMTF.Decode.MessagePack
import           Bio.MMTF.Type

import           Data.Map.Strict             (Map)
import           Data.MessagePack            (Object)
import           Data.Text                   (Text)

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

-- |Parses structure data from ObjectMap
structureData :: Monad m => Map Text Object -> m StructureData
structureData mp = do ttl' <- atPM mp "title" asStr
                      sid' <- atPM mp "structureId" asStr
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
                      btl' <- (codec4 . parseBinary <$>) <$> atPM mp "bondAtomList" asBinary
                      bol' <- (codec2 . parseBinary <$>) <$> atPM mp "bondOrderList" asBinary
                      pure $ StructureData ttl' sid' dd' rd' nb' na' ng' nc' nm' sg' uc' nol'
                                           Nothing Nothing res' rf' rw' em' btl' bol'
