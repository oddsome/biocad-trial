{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeApplications  #-}


module Lib where

import Database.Bolt (BoltActionT)
import Database.Bolt.Extras.Graph
import Control.Monad.State (execState, State)
import Data.Text (Text, pack)
import GHC.Records
import Language.Haskell.TH
import Test.QuickCheck (Arbitrary(arbitrary))




putGraph :: State GraphPutRequest () -> BoltActionT IO [Graph NodeName (NodeRes PutRequest) (RelRes PutRequest)]
putGraph = makeRequest @PutRequest [] . flip execState emptyGraph

getGraph :: State GraphGetRequest () -> BoltActionT IO [Graph NodeName (NodeRes GetRequest) (RelRes GetRequest)]
getGraph = makeRequest @GetRequest [] . flip execState emptyGraph

-- node name to reference in cypher
getN :: HasField "id" r Int => r -> Text
getN = pack . ('n':) . show . getField @"id"

-- instance Arbitrary codegen
----------------------------------------------------------------------------------
makeArbitrary :: Name -> Q [Dec]
makeArbitrary typeCon = do
        TyConI dec               <- reify typeCon
        let tyName               = getTypeCons dec  
        let (dataCon, fieldsNum) = getConAndFieldsNum tyName
        arbClause                <- makeArbClause dataCon fieldsNum
        let bodyDecl             = [FunD 'arbitrary [arbClause]]
        pure [InstanceD Nothing [] (AppT (ConT ''Arbitrary) (ConT typeCon)) bodyDecl]
    where
        getConAndFieldsNum :: Con -> (Name, Int)
        getConAndFieldsNum (RecC dataCon decs)    = (dataCon, length decs)
        getConAndFieldsNum (NormalC dataCon cons) = (dataCon, length cons)
        getConAndFieldsNum _                      = error "unsupported data declaration."

        -- Single constructor data only
        getTypeCons :: Dec -> Con
        getTypeCons (DataD    _ typeName _ _ constructors _) = head constructors
        getTypeCons _                                        = error "unsupported type"

        makeArbClause :: Name -> Int -> Q Clause
        makeArbClause valCon fieldsNum = do
            pure $ Clause [] (NormalB body) []
                where
                    body = foldl
                        (\a _ -> AppE (AppE (VarE '(<*>)) a) (VarE 'arbitrary)) 
                        (AppE (VarE 'pure) (ConE valCon))
                        [1..fieldsNum]
