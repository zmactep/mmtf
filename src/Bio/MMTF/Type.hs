module Bio.MMTF.Type where

import           Data.Int  (Int32, Int8)
import           Data.Text (Text)

-- |Unit cell data
data UnitCell = UnitCell { ucA     :: !Float -- ^length of side 'a'
                         , ucB     :: !Float -- ^length of side 'b'
                         , ucC     :: !Float -- ^length of side 'c'
                         , ucAlpha :: !Float -- ^alpha angle in degrees
                         , ucBeta  :: !Float -- ^beta angle in degrees
                         , ucGamma :: !Float -- ^gamma angle in degrees
                         }
  deriving (Show, Eq)

-- |Transform data
data Transform = Transform { chainIndexList :: ![Int32] -- ^indices into the 'chainIdList' and 'chainNameList' fields
                           , matrix         :: ![Float] -- ^4x4 transformation matrix
                           }
  deriving (Show, Eq)

-- |Assembly data
data Assembly = Assembly { transformList :: ![Transform] -- ^List of transform objects
                         , assemblyName  :: !Text        -- ^Name of the biological assembly
                         }
  deriving (Show, Eq)

-- |Entity data
data Entity = Entity { entityChainIndexList :: ![Int32] -- ^indices into the 'chainIdList' and 'chainNameList' fields
                     , entityDescription    :: !Text    -- ^Description of the entity
                     , entityType           :: !Text    -- ^Name of the entity type
                     , entitySequence       :: !Text    -- ^Sequence of the full construct in one-letter-code
                     }
  deriving (Show, Eq)

-- |Group type data
data GroupType = GroupType { gtFormalChargeList :: ![Int32] -- ^List of formal charges
                           , gtAtomNameList     :: ![Text]  -- ^List of atom names
                           , gtElementList      :: ![Text]  -- ^List of elements
                           , gtBondAtomList     :: ![Int32] -- ^List of bonded atom indices
                           , gtBondOrderList    :: ![Int32] -- ^List of bond orders
                           , gtGroupName        :: !Text    -- ^The name of the group
                           , gtSingleLetterCode :: !Char    -- ^The single letter code
                           , gtChemCompType     :: !Text    -- ^The chemical component type
                           }
  deriving (Show, Eq)

-- |Protein secondary structure
data SecondaryStructure = PiHelix       -- ^pi helix
                        | Bend          -- ^bend
                        | AlphaHelix    -- ^alpha helix
                        | Extended      -- ^extended
                        | ThreeTenHelix -- ^3-10 helix
                        | Bridge        -- ^brigde
                        | Turn          -- ^turn
                        | Coil          -- ^coil
                        | Undefined     -- ^unknown structure
  deriving (Show, Eq)

-- |MMTF format data
data FormatData = FormatData { mmtfVersion  :: !Text -- ^The version number of the specification the file adheres to
                             , mmtfProducer :: !Text -- ^The name and version of the software used to produce the file
                             }
  deriving (Show, Eq)

-- |Structure data
data StructureData = StructureData { title               :: !(Maybe Text)       -- ^A short description of the structural data included in the file
                                   , structureId         :: !(Maybe Text)       -- ^An ID for the structure, for example the PDB ID if applicable
                                   , depositionDate      :: !(Maybe Text)       -- ^A date that relates to the deposition of the structure in a database
                                   , releaseDate         :: !(Maybe Text)       -- ^A date that relates to the release of the structure in a database
                                   , numBonds            :: !Int32              -- ^The overall number of bonds
                                   , numAtoms            :: !Int32              -- ^The overall number of atoms in the structure
                                   , numGroups           :: !Int32              -- ^The overall number of groups in the structure
                                   , numChains           :: !Int32              -- ^The overall number of chains in the structure
                                   , numModels           :: !Int32              -- ^The overall number of models in the structure
                                   , spaceGroup          :: !(Maybe Text)       -- ^The Hermann-Mauguin space-group symbol
                                   , unitCell            :: !(Maybe UnitCell)   -- ^Array of six values defining the unit cell
                                   , ncsOperatorList     :: !(Maybe [[Float]])  -- ^List of lists representing 4x4 transformation matrices that are stored linearly in row major order (transformation matrices describe noncrystallographic symmetry operations needed to create all molecules in the unit cell)
                                   , bioAssemblyList     :: !(Maybe [Assembly]) -- ^List of instructions on how to transform coordinates for an array of chains to create (biological) assemblies
                                   , entityList          :: !(Maybe [Entity])   -- ^List of unique molecular entities within the structure
                                   , resolution          :: !(Maybe Float)      -- ^The experimental resolution in Angstrom
                                   , rFree               :: !(Maybe Float)      -- ^The R-free value
                                   , rWork               :: !(Maybe Float)      -- ^The R-work value
                                   , experimentalMethods :: !(Maybe [Text])     -- ^List of experimental methods employed for structure determination
                                   , bondAtomList        :: !(Maybe [Int32])    -- ^Pairs of values represent indices of covalently bonded atoms [binary (type 4)]
                                   , bondOrderList       :: !(Maybe [Int8])     -- ^List of bond orders for bonds in 'bondAtomList' [binary (type 2)]
                                   }
  deriving (Show, Eq)

-- |Models data
data ModelData = ModelData { chainsPerModel :: ![Int32] -- ^List of the number of chains in each model
                           }
  deriving (Show, Eq)

-- |Chains data
data ChainData = ChainData { groupsPerChain :: ![Int32]        -- ^List of the number of groups (aka residues) in each chain
                           , chainIdList    :: ![Text]         -- ^List of chain IDs [binary (type 5)]
                           , chainNameList  :: !(Maybe [Text]) -- ^List of chain names [binary (type 5)]
                           }
  deriving (Show, Eq)

-- |Groups data
data GroupData = GroupData { groupList         :: ![GroupType]                  -- ^List of groupType objects
                           , groupTypeList     :: ![Int32]                      -- ^List of pointers to 'groupType' entries in 'groupList' by their keys[binary (type 4)]
                           , groupIdList       :: ![Int32]                      -- ^List of group (residue) numbers [binary (type 8)]
                           , secStructList     :: !(Maybe [SecondaryStructure]) -- ^List of secondary structure assignments [binary (type 2)]
                           , insCodeList       :: !(Maybe [Char])               -- ^List of insertion codes, one for each group (residue) [binary (type 6)]
                           , sequenceIndexList :: !(Maybe [Int32])              -- ^List of indices that point into the sequence property of an entity object in the 'entityList' field that is associated with the chain the group belongs to [binary (type 8)]
                           }
  deriving (Show, Eq)

-- |Atoms data
data AtomData = AtomData { atomIdList    :: !(Maybe [Int32]) -- ^List of atom serial numbers [binary (type 8)]
                         , altLocList    :: !(Maybe [Char])  -- ^List of alternate location labels, one for each atom [binary (type 6)]
                         , bFactorList   :: !(Maybe [Float]) -- ^List of atom B-factors in in A^2, one for each atom [binary (type 10)]
                         , xCoordList    :: ![Float]         -- ^List of x atom coordinates in A, one for each atom [binary (type 10)]
                         , yCoordList    :: ![Float]         -- ^List of y atom coordinates in A, one for each atom [binary (type 10)]
                         , zCoordList    :: ![Float]         -- ^List of z atom coordinates in A, one for each atom [binary (type 10)]
                         , occupancyList :: !(Maybe [Float]) -- ^List of atom occupancies, one for each atom [binary (type 9)]
                         }
  deriving (Show, Eq)

-- |MMTF datatype
data MMTF = MMTF { format    :: !FormatData    -- ^MMTF format data
                 , structure :: !StructureData -- ^Biological structure data
                 , model     :: !ModelData     -- ^Models data
                 , chain     :: !ChainData     -- ^Chains data
                 , group     :: !GroupData     -- ^Groups data
                 , atom      :: !AtomData      -- ^Atoms data
                 }
  deriving (Show, Eq)
