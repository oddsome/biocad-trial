{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeApplications  #-}


module Lib where

import Database.Bolt (BoltActionT)
import Database.Bolt.Extras
import Database.Bolt.Extras.Graph
import Data.List (foldl')
import Data.Map (Map, filterWithKey, unionWith, empty, member, fromList, toList, (!))
import Control.Lens (at, non, to, (^.), over)
import Control.Monad.State (execState, State)
import Data.Text (Text, pack, unpack)
import GHC.Records
import Language.Haskell.TH
import Test.QuickCheck (Arbitrary(arbitrary))



mergeG :: (Eq a, Eq b) => [Graph NodeName a b] -> Graph NodeName [a] [b]
mergeG = foldl' mergeGraph emptyGraph
    where
        mergeMap :: (Ord k, Eq v) => Map k [v] -> Map k v -> Map k [v]
        mergeMap big small = unionWith (<>) big $ (:[]) <$> filterWithKey checkElem small
                where
                    checkElem k v | k `member` big = v `notElem` big ! k
                                  | otherwise      = True

        mergeGraph :: (Eq a, Eq b) => Graph NodeName [a] [b] -> Graph NodeName a b -> Graph NodeName [a] [b]
        mergeGraph big small = over relations (mergeMap (big ^. relations)) $
                                            over vertices  (mergeMap (big ^. vertices))
                                            small

extractNodes :: NodeLike a => NodeName -> Graph NodeName [NodeResult] [RelResult] -> [a]
extractNodes var graph = graph ^. vertices . at var . non (errorForNode var) . to (fmap $ fromNode . toNode)
    where
        errorForNode :: NodeName -> a
        errorForNode name = error . unpack $ "node with name " <> name <> " doesn't exist"

extractRelations :: URelationLike a => NodeName -> NodeName -> Graph NodeName [NodeResult] [RelResult] -> [a]
extractRelations stVar enVar graph = graph ^. relations . at (stVar, enVar)
                                  . non (errorForRelation stVar enVar)
                                  . to (fmap $ fromURelation . toURelation)
    where
        errorForRelation :: NodeName -> NodeName -> a
        errorForRelation stName enName = error . unpack $ "relation between nodes " <>
                                                        stName <> " and " <> enName <>
                                                        " doesn't exist"

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
