{- 
 - Zachary Weaver (C) 2012
 - Template.hs
 -
 - Provides a template for HaskellDB boilerplate
 -}

{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
{-|
  This is a rework of the interface for Justin Bailey's
  haskelldb-th package.  It's designed to be easier to 
  use, but allow for flexibility.  This is done via intermediate
  types that can be manipulated via lenses before generating the
  actual code.

  Typically, it will be used simply like so

  > genTableAndFields $ mkTable "MyTable"
  >   [ "Key"   .:: [t|Int|]
  >   , "User"  .:: [t|String|]
  >   , "Hash"  .:: [t|String|]
  >   ]

  which generates code like

  > type MyTable = RecCons Key
  >                        (Expr Int)
  >                        (RecCons User
  >                                 (Expr String)
  >                                 (RecCons Hash
  >                                          (Expr String)
  >                                          RecNil))
  > myTable :: Table MyTable
  > myTable = baseTable "MyTable" (hdbMakeEntry Key # (hdbMakeEntry User # hdbMakeEntry Hash))
  > data Key = Key
  > instance FieldTag Key
  >     where fieldName _ = "Key"
  > key :: Attr Key Int
  > key = mkAttr Key
  > data User = User
  > instance FieldTag User
  >     where fieldName _ = "User"
  > user :: Attr User String
  > user = mkAttr User
  > data Hash = Hash
  > instance FieldTag Hash
  >     where fieldName _ = "Hash"
  > hash :: Attr Hash String
  > hash = mkAttr Hash

  (The actual generated code would befully qualified)
  
  It's that easy.
-}
module Database.HaskellDB.Template 
    ( Names
    , mkNames
    , typeName
    , varName

    , Field
    , mkField
    , (.::)
    , fieldNames
    , fieldInDB
    , fieldTypeQ

    , Table
    , mkTable
    , tableNames
    , tableInDB
    , tableFields

    , genField
    , genTable
    , genTableAndFields
    ) where

import           Control.Lens
import           Control.Monad
import           Data.List
import           Data.Char
import           Language.Haskell.TH

import qualified Database.HaskellDB as HDB
import qualified Database.HaskellDB.HDBRec as HDB
import qualified Database.HaskellDB.DBLayout as HDB

data Names = Names
    { _typeName :: Name
    , _varName :: Name
    }
makeLenses ''Names

data Field = Field
    { _fieldNames :: Names
    , _fieldInDB :: String
    , _fieldTypeQ :: TypeQ
    }
makeLenses ''Field

data Table = Table
    { _tableNames :: Names
    , _tableInDB :: String
    , _tableFields :: [Field]
    }
makeLenses ''Table

{-|
  This will create a type and variable from a given string that are identical
  save for being properly captalized.
-}
mkNames :: String -> Names
mkNames "" = error "Database.HaskellDB.Template: Empty names passed to mkNames"
mkNames (c:cs) = Names
    { _typeName = mkName $ toUpper c : cs
    , _varName = mkName $ toLower c : cs
    }

{-| 
  A shortcut function to build a 'Field' from just a name
  and a type.  For more complicated needs, use the
  type constructor.
-}
mkField :: String -> TypeQ -> Field
mkField name typeQ = Field
    { _fieldNames = mkNames name
    , _fieldInDB = name
    , _fieldTypeQ = typeQ
    }

(.::) :: String -> TypeQ -> Field
(.::) = mkField
infix 1 .::

{-| 
  A shortcut function to build a 'Table' from just a name
  and a list of 'Field's.  For more complicated needs, use the
  type constructor.
-}
mkTable :: String -> [Field] -> Table
mkTable name fields = Table
    { _tableNames = mkNames name
    , _tableInDB = name
    , _tableFields = fields
    }

{-|
  Generates a field declaration from a 'Field' type
-}
genField :: Field -> Q [Dec]
genField field = do
    colType <- field^.fieldTypeQ
    return 
        [ DataD [] fieldType [] [NormalC fieldType []] []
        , InstanceD [] (foldr1 AppT $ map ConT [''HDB.FieldTag, fieldType])
            [ FunD 'HDB.fieldName [Clause [WildP] (NormalB
                $ LitE $ StringL $ field^.fieldInDB) []]
            ]
        , SigD fieldName $ foldl1 AppT $ map ConT 
            [''HDB.Attr, fieldType] ++ [colType]
        , flip (ValD $ VarP fieldName) [] $ NormalB $ AppE
            (VarE 'HDB.mkAttr) $ ConE fieldType
        ]
  where fieldType = field^.fieldNames.typeName
        fieldName = field^.fieldNames.varName

{-|
  Generates a table declaration from a 'Table' type
-}
genTable :: Table -> Q [Dec]
genTable table = do
    let tableType = table^.tableNames.typeName
    let tableName = table^.tableNames.varName
    recCons <- foldM recConQ (ConT ''HDB.RecNil) $ fields
    return 
        [ TySynD tableType [] recCons
        , SigD tableName $ AppT (ConT ''HDB.Table) $ ConT tableType
        , flip (ValD $ VarP tableName) [] $ NormalB $ foldl1 AppE
            [ VarE 'HDB.baseTable
            , LitE $ StringL $ table^.tableInDB
            , foldl1 (\x acc -> InfixE (Just acc) (VarE $ mkName "#") (Just x))
                $ map (AppE (VarE 'HDB.hdbMakeEntry)  
                    . ConE . view (fieldNames.typeName)) fields
            ]
        ]
  where fields = reverse $ table^.tableFields
        recConQ recCon field = do
            colType <- field^.fieldTypeQ
            return $ foldl1 AppT 
                [ ConT ''HDB.RecCons
                , ConT $ field^.fieldNames.typeName
                , AppT (ConT ''HDB.Expr) colType
                , recCon
                ]
            

{-|
  Generates both a table and all its fields from a single Table type.
-}
genTableAndFields :: Table -> Q [Dec]
genTableAndFields table = do
    tableDecl <- genTable table
    fieldsDecl <- fmap concat $ mapM genField $ table^.tableFields
    return $ tableDecl ++ fieldsDecl
