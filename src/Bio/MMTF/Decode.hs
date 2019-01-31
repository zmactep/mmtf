{-# LANGUAGE OverloadedStrings #-}

module Bio.MMTF.Decode where

import           Bio.MMTF.Decode.Codec
import           Bio.MMTF.Decode.MessagePack
import           Bio.MMTF.Type

import           Control.Monad               ((>=>))
import           Data.ByteString.Lazy        (empty)
import           Data.Map.Strict             (Map)
import           Data.MessagePack            (Object)
import           Data.Text                   (Text)

-- | Parses format data from ObjectMap
--
formatData :: Monad m => Map Text Object -> m FormatData
formatData mp = do v <- atP mp "mmtfVersion" asStr
                   p <- atP mp "mmtfProducer" asStr
                   pure $ FormatData v p

-- | Parses model data from ObjectMap
--
modelData :: Monad m => Map Text Object -> m ModelData
modelData mp = ModelData <$> atP mp "chainsPerModel" asIntList

-- | Parses chain data from ObjectMap
--
chainData :: Monad m => Map Text Object -> m ChainData
chainData mp = do gpc <- atP mp "groupsPerChain" asIntList
                  cil <- codec5 . parseBinary <$> atP   mp "chainIdList"   asBinary
                  cnl <- codec5 . parseBinary <$> atPMD mp "chainNameList" asBinary empty
                  pure $ ChainData gpc cil cnl

-- | Parses atom data from ObjectMap
--
atomData :: Monad m => Map Text Object -> m AtomData
atomData mp = do ail' <-  codec8 . parseBinary <$> atPMD mp "atomIdList"    asBinary empty
                 all' <-  codec6 . parseBinary <$> atPMD mp "altLocList"    asBinary empty
                 bfl' <- codec10 . parseBinary <$> atPMD mp "bFactorList"   asBinary empty
                 xcl' <- codec10 . parseBinary <$> atP   mp "xCoordList"    asBinary
                 ycl' <- codec10 . parseBinary <$> atP   mp "yCoordList"    asBinary
                 zcl' <- codec10 . parseBinary <$> atP   mp "zCoordList"    asBinary
                 ol' <-   codec9 . parseBinary <$> atPMD mp "occupancyList" asBinary empty
                 pure $ AtomData ail' all' bfl' xcl' ycl' zcl' ol'

-- | Parses group data from ObjectMap
--
groupData :: Monad m => Map Text Object -> m GroupData
groupData mp = do gl' <- atP mp "groupList" asObjectList >>= traverse (transformObjectMap >=> groupType)
                  gtl' <-              codec4 . parseBinary <$> atP   mp "groupTypeList"     asBinary
                  gil' <-              codec8 . parseBinary <$> atP   mp "groupIdList"       asBinary
                  ssl' <- fmap ssDec . codec2 . parseBinary <$> atPMD mp "secStructList"     asBinary empty
                  icl' <-              codec6 . parseBinary <$> atPMD mp "insCodeList"       asBinary empty
                  sil' <-              codec8 . parseBinary <$> atPMD mp "sequenceIndexList" asBinary empty
                  pure $ GroupData gl' gtl' gil' ssl' icl' sil'

-- | Parses group type from ObjectMap
--
groupType :: Monad m => Map Text Object -> m GroupType
groupType mp = do fcl' <- atP mp "formalChargeList" asIntList
                  anl' <- atP mp "atomNameList"     asStrList
                  el'  <- atP mp "elementList"      asStrList
                  bal' <- atP mp "bondAtomList"     asIntList
                  bol' <- atP mp "bondOrderList"    asIntList
                  gn'  <- atP mp "groupName"        asStr
                  slc' <- atP mp "singleLetterCode" asChar
                  cct' <- atP mp "chemCompType"     asStr
                  pure $ GroupType fcl' anl' el' bal' bol' gn' slc' cct'

-- | Parses structure data from ObjectMap
--
structureData :: Monad m => Map Text Object -> m StructureData
structureData mp = do ttl' <-                          atPMD mp "title"               asStr        ""
                      sid' <-                          atPMD mp "structureId"         asStr        ""
                      dd'  <-                          atPMD mp "depositionDate"      asStr        ""
                      rd'  <-                          atPMD mp "releaseDate"         asStr        ""
                      nb'  <-                          atP   mp "numBonds"            asInt
                      na'  <-                          atP   mp "numAtoms"            asInt
                      ng'  <-                          atP   mp "numGroups"           asInt
                      nc'  <-                          atP   mp "numChains"           asInt
                      nm'  <-                          atP   mp "numModels"           asInt
                      sg'  <-                          atPMD mp "spaceGroup"          asStr        ""
                      uc'  <-          (>>= ucDec) <$> atPM  mp "unitCell"            asFloatList
                      nol' <-    (>>= asFloatList) <$> atPMD mp "ncsOperatorList"     asObjectList []
                      bal' <-                          atPMD mp "bioAssemblyList"     asObjectList [] >>= traverse (transformObjectMap >=> bioAssembly)
                      el'  <-                          atPMD mp "entityList"          asObjectList [] >>= traverse (transformObjectMap >=> entity)
                      res' <-                          atPM  mp "resolution"          asFloat
                      rf'  <-                          atPM  mp "rFree"               asFloat
                      rw'  <-                          atPM  mp "rWork"               asFloat
                      em'  <-                          atPMD mp "experimentalMethods" asStrList []
                      btl' <- codec4 . parseBinary <$> atPMD mp "bondAtomList"        asBinary empty
                      bol' <- codec2 . parseBinary <$> atPMD mp "bondOrderList"       asBinary empty
                      pure $ StructureData ttl' sid' dd' rd' nb' na' ng' nc' nm' sg' uc' nol'
                                           bal' el' res' rf' rw' em' btl' bol'

-- | Parses bio assembly data from ObjectMap
--
bioAssembly :: Monad m => Map Text Object -> m Assembly
bioAssembly mp = do nme' <- atP mp "name"          asStr
                    tlt' <- atP mp "transformList" asObjectList >>= traverse (transformObjectMap >=> transform)
                    pure $ Assembly tlt' nme'

-- | Parses transform data from ObjectMap
--
transform :: Monad m => Map Text Object -> m Transform
transform mp = do cil' <- atP mp "chainIndexList" asIntList
                  mtx' <- atP mp "matrix"         asFloatList
                  pure $ Transform cil' mtx'

-- | Parses entity data from ObjectMap
--
entity :: Monad m => Map Text Object -> m Entity
entity mp = do cil' <- atP mp "chainIndexList" asIntList
               dsc' <- atP mp "description"    asStr
               tpe' <- atP mp "type"           asStr
               sqc' <- atP mp "sequence"       asStr
               pure $ Entity cil' dsc' tpe' sqc'
