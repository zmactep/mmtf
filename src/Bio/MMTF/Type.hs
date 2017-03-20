module Bio.MMTF.Type where

import           Data.Int  (Int32, Int8)
import           Data.Text (Text)

data UnitCell = UnitCell { ucA     :: !Float
                         , ucB     :: !Float
                         , ucC     :: !Float
                         , ucAlpha :: !Float
                         , ucBeta  :: !Float
                         , ucGamma :: !Float
                         }
  deriving (Show, Eq)

data Transform = Transform { chainIndexList :: ![Int32]
                           , matrix         :: ![Float]
                           }
  deriving (Show, Eq)

data Assembly = Assembly { transformList :: ![Transform]
                         , assemblyName  :: !Text
                         }
  deriving (Show, Eq)

data Entity = Entity { entityChainIndexList :: ![Int32]
                     , entityDescription    :: !Text
                     , entityType           :: !Text
                     , entitySequence       :: !Text
                     }
  deriving (Show, Eq)

data GroupType = GroupType { gtFormalChargeList :: ![Int32]
                           , gtAtomNameList     :: ![Text]
                           , gtElementList      :: ![Text]
                           , gtBondAtomList     :: ![Int32]
                           , gtBondOrderList    :: ![Int32]
                           , gtGroupName        :: !Text
                           , gtSingleLetterCode :: !Char
                           , gtChemCompType     :: !Text
                           }
  deriving (Show, Eq)

data SecondaryStructure = PiHelix       -- 0
                        | Bend          -- 1
                        | AlphaHelix    -- 2
                        | Extended      -- 3
                        | ThreeTenHelix -- 4
                        | Bridge        -- 5
                        | Turn          -- 6
                        | Coil          -- 7
                        | Undefined     -- -1
  deriving (Show, Eq)

data FormatData = FormatData { mmtfVersion  :: !Text
                             , mmtfProducer :: !Text
                             }
  deriving (Show, Eq)

data StructureData = StructureData { title               :: !(Maybe Text)
                                   , strucutreId         :: !(Maybe Text)
                                   , depositionDate      :: !(Maybe Text)
                                   , releaseDate         :: !(Maybe Text)
                                   , numBonds            :: !Int32
                                   , numAtoms            :: !Int32
                                   , numGroups           :: !Int32
                                   , numChains           :: !Int32
                                   , numModels           :: !Int32
                                   , spaceGroup          :: !(Maybe Text)
                                   , unitCell            :: !(Maybe UnitCell)
                                   , ncsOperatorList     :: !(Maybe [[Float]])
                                   , bioAssemblyList     :: !(Maybe ())--[Assembly])
                                   , entityList          :: !(Maybe ())--[Entity])
                                   , resolution          :: !(Maybe Float)
                                   , rFree               :: !(Maybe Float)
                                   , rWork               :: !(Maybe Float)
                                   , experimentalMethods :: !(Maybe [Text])
                                   , bondAtomList        :: !(Maybe [Int32]) -- binary (type 4)
                                   , bondOrderList       :: !(Maybe [Int8])  -- binary (type 2)
                                   }
  deriving (Show, Eq)

data ModelData = ModelData { chainsPerModel :: ![Int32] }
  deriving (Show, Eq)

data ChainData = ChainData { groupsPerChain :: ![Int32]
                           , chainIdList    :: ![Text] -- binary (type 5, length 4)
                           , chainNameList  :: !(Maybe [Text]) -- binary (type 5, length 4)
                           }
  deriving (Show, Eq)

data GroupData = GroupData { groupList         :: ![GroupType]
                           , groupTypeList     :: ![Int32] -- binary (type 4)
                           , groupIdList       :: ![Int32] -- binary (type 8)
                           , secStructList     :: !(Maybe [SecondaryStructure]) -- binary (type 2)
                           , insCodeList       :: !(Maybe [Char]) -- binary (type 6)
                           , sequenceIndexList :: !(Maybe [Int32]) -- binary (type 8)
                           }
  deriving (Show, Eq)

data AtomData = AtomData { atomIdList    :: !(Maybe [Int32]) -- binary (type 8)
                         , altLocList    :: !(Maybe [Char]) -- binary (type 6)
                         , bFactorList   :: !(Maybe [Float]) -- binary (type 10)
                         , xCoordList    :: ![Float] -- binary (type 10)
                         , yCoordList    :: ![Float] -- binary (type 10)
                         , zCoordList    :: ![Float] -- binary (type 10)
                         , occupancyList :: !(Maybe [Float]) -- binary (type 9)
                         }
  deriving (Show, Eq)

data MMTF = MMTF { format    :: !FormatData
                 , structure :: !StructureData
                 , model     :: !ModelData
                 , chain     :: !ChainData
                 , group     :: !GroupData
                 , atom      :: !AtomData
                 }
  deriving (Show, Eq)
