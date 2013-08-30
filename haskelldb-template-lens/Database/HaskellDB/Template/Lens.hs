{-|
  This provides lenses for the data types in the main module.
  It's provided separately in case one wants to avoid the Data.Lens
  dependency
-}
module Database.HaskellDB.Template.Lens where

import Data.Lens.Common

import Database.HaskellDB.Template

-- In theory we could use Templates here, but meh

typeNameL :: Lens Names Name
typeNameL = lens typeName $
    \name names -> names{typeName = name}

varNameL :: Lens Names Name
varNameL = lens varName $
    \name names -> names{varName = name}

fieldNamesL :: Lens Field Names
fieldNamesL = lens fieldInDB $
    \names field -> field{fieldNames = names}

fieldInDBL :: Lens Field String
fieldInDBL = lens fieldInDB $
    \inDB field -> field{fieldInDB = inDB}

fieldTypeQL :: Lens Field TypeQ
fieldTypeQL = lens fieldInDB $
    \typeQ field -> field{fieldTypeQ = typeQ}

tableNamesL :: Lens Table Names
tableNamesL = lens tableInDB $
    \names table -> table{tableNames = names}

tableInDBL :: Lens Table String
tableInDBL = lens tableInDB $
    \inDB table -> table{tableInDB = inDB}

tableFields :: Lens Table [Field]
tableFields = lens tableFields $
    \fields table -> table{tableFields = fields}
