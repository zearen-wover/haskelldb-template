{-# LANGUAGE TemplateHaskell #-}
{-|
  This provides lenses for the data types in the main module.
  It's provided separately in case one wants to avoid the Data.Lens
  dependency
-}
module Database.HaskellDB.Template.Data.Lens where

import Language.Haskell.TH
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
fieldNamesL = lens fieldNames $
    \names field -> field{fieldNames = names}

fieldInDBL :: Lens Field String
fieldInDBL = lens fieldInDB $
    \inDB field -> field{fieldInDB = inDB}

fieldTypeQL :: Lens Field TypeQ
fieldTypeQL = lens fieldTypeQ $
    \typeQ field -> field{fieldTypeQ = typeQ}

tableNamesL :: Lens Table Names
tableNamesL = lens tableNames $
    \names table -> table{tableNames = names}

tableInDBL :: Lens Table String
tableInDBL = lens tableInDB $
    \inDB table -> table{tableInDB = inDB}

tableFieldsL :: Lens Table [Field]
tableFieldsL = lens tableFields $
    \fields table -> table{tableFields = fields}
