{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleContexts      #-}


module Main where

import Lib

import Data.Map (Map, filterWithKey, unionWith, empty, member, fromList, toList, (!))
import Data.List (zipWith4, foldl')
import Data.Text (Text, pack)
import Data.Maybe (fromJust)
import Data.Default

import Database.Bolt
import Database.Bolt.Extras
import Database.Bolt.Extras.DSL (NodeSelector (..))
import Database.Bolt.Extras.Graph
import Database.Bolt.Extras.Template

import Control.Monad.State
import Control.Exception (bracket)

import Test.QuickCheck (elements, generate, listOf1, shuffle, Arbitrary(arbitrary), Gen)



-- вспомогательные штуки
instance Arbitrary Text where
    arbitrary = fmap pack $ listOf1 $ elements "bedali bedali sunky"

boltCfg :: BoltCfg
boltCfg = def { host = "localhost"
              , user = "neo4j"
              , password = "test"
              }

runQueryDB :: BoltActionT IO a -> IO a
runQueryDB act = bracket (connect boltCfg) close (`run` act)

clearDB :: IO ()
clearDB = runQueryDB $ query_ "MATCH (n) DETACH DELETE n"



-- создайте соответствующие типы в Haskell-библиотеке
data Molecule = Molecule {id :: Int, smiles :: Text, iupacName :: Text} deriving (Eq, Show)
makeNodeLike  ''Molecule

data Reaction = Reaction {id :: Int, name :: Text} deriving (Eq, Show)
makeNodeLike  ''Reaction

data Catalyst = Catalyst {id :: Int, smiles :: Text, name :: Maybe Text} deriving (Eq, Show)
makeNodeLike  ''Catalyst

data PRODUCT_FROM = PRODUCT_FROM {amount :: Float} deriving (Eq, Show)
makeURelationLike ''PRODUCT_FROM
makeArbitrary     ''PRODUCT_FROM

data ACCELERATE = ACCELERATE {temperature :: Float, pressure :: Float} deriving (Eq, Show)
makeURelationLike ''ACCELERATE
makeArbitrary     ''ACCELERATE

data REAGENT_IN = REAGENT_IN deriving Show
makeURelationLike ''REAGENT_IN



-- населите базу данных представителями (хотя бы по 20 образцов каждого вида).
generateTestData :: Int -> 
                    Int -> 
                    IO ([[Molecule]], [(Catalyst, ACCELERATE)], [Reaction], [[(Molecule, PRODUCT_FROM)]])
generateTestData from howMuch = flip evalStateT from $ do
        ms <- replicateM (howMuch * 2) $ gen $ \i -> Molecule i <$> arbitrary <*> arbitrary
        
        (splitBy2 -> mIns) <- lgen $ shuffle ms

        reactions   <- replicateM howMuch $ gen $ \i -> Reaction i <$> arbitrary 

        catalysts   <- replicateM howMuch $ gen $ \i -> Catalyst i <$> arbitrary <*> arbitrary
        accelerates <- replicateM howMuch $ lgen $ arbitrary @ACCELERATE

        (take howMuch -> mOuts) <- lgen $ shuffle ms
        pFroms      <- replicateM howMuch $ lgen $ arbitrary @PRODUCT_FROM

        pure (mIns, zip catalysts accelerates, reactions, (:[]) <$> zip mOuts pFroms)
    where
        lgen = lift . generate

        gen :: (Int -> Gen a) -> StateT Int IO a
        gen f = do
            i <- get
            modify succ
            lgen $ f i

        splitBy2 (f:s:r) = [f,s] : splitBy2 r
        splitBy2 _       = []

putTestData :: IO [[Graph NodeName BoltId BoltId]]
putTestData = do
        (mIns, cas, rs, pFs) <- generateTestData 0 50
        sequence $ zipWith4 putWholeReaction mIns cas rs pFs



-- напишите функцию, которая умеет принимать реацию на вход и загружать её в базу
addWholeReactionG :: [Molecule] -> 
                    (Catalyst, ACCELERATE) ->
                    Reaction ->
                    [(Molecule, PRODUCT_FROM)] ->
                    State GraphPutRequest ()
addWholeReactionG mIns (catalyst, accelerate) reaction (unzip -> (mOuts, product_froms)) = do 
        newNode reaction

        newNode catalyst
        newRelation accelerate reaction catalyst

        sequence_ $ newNode <$> mIns
        sequence_ $ newRelation REAGENT_IN reaction <$> mIns

        sequence_ $ newNode <$> mOuts
        sequence_ $ zipWith3 newRelation product_froms mOuts (repeat reaction)
    where
        newNode n = unless (member (getN n) . _vertices) $
            addNode (getN n) $ MergeN $ toNode n

        newRelation r nTo nFrom = unless (member (getN nFrom, getN nTo) . _relations) $
            addRelation (getN nFrom) (getN nTo) $ CreateR $ toURelation r

        unless condFun action = do
            (condFun -> cond) <- get
            if cond
                then pure ()
                else modify action

putWholeReaction :: [Molecule] ->
               (Catalyst, ACCELERATE) ->
               Reaction ->
               [(Molecule, PRODUCT_FROM)] ->
               IO [Graph NodeName (NodeRes PutRequest) (RelRes PutRequest)]
putWholeReaction mIns cas reaction mOutsPfs = runQueryDB $ putGraph $ addWholeReactionG mIns cas reaction mOutsPfs



-- напишите функцию, которая по номеру реакции в базе будет возвращать её в Haskell-объект
matchWholeReactionG :: Int -> State GraphGetRequest ()
matchWholeReactionG rId = do
        getNode "reaction"            ''Reaction $ withProp ("id", I rId)
        
        getNode "mIns"                ''Molecule        plain
        modify $ addRelation "mIns" "reaction" $ defaultRelNotReturn # withLabelQ ''REAGENT_IN

        getNode "catalyst"            ''Catalyst        plain
        getRel  "reaction" "catalyst" ''ACCELERATE      plain

        getNode "mOuts"               ''Molecule        plain
        getRel  "mOuts" "reaction"    ''PRODUCT_FROM    plain
    where
        plain = Prelude.id

        getNode nVar label params = modify $ addNode nVar $ 
            defaultNodeReturn # 
                withLabelQ label #
                withReturn allProps #
                params

        getRel nTo nFrom label params = modify $ addRelation nFrom nTo $
            defaultRelReturn #
            withLabelQ label #
            withReturn allProps #
            params

-- боллее медленная, но приятно выглядящая версия
getWholeReactionSlow :: Int -> IO ([Molecule], (Catalyst, ACCELERATE), Reaction, [(Molecule, PRODUCT_FROM)])
getWholeReactionSlow rId = do
    (mergeG -> g) <- runQueryDB $ getGraph $ matchWholeReactionG rId

    let mIns@[_,_]   = extractNodes     "mIns" g
    let [reaction]   = extractNodes     "reaction" g
    let [catalyst]   = extractNodes     "catalyst" g
    let [accelerate] = extractRelations "catalyst" "reaction" g
    let mOuts@[_]    = extractNodes     "mOuts" g
    let pFs@[_]      = extractRelations "reaction" "mOuts" g

    pure (mIns, (catalyst, accelerate), reaction, zip mOuts pFs)

-- обычная версия
getWholeReaction :: Int -> IO ([Molecule], (Catalyst, ACCELERATE), Reaction, [(Molecule, PRODUCT_FROM)])
getWholeReaction rId = do
    gs <- runQueryDB $ getGraph $ matchWholeReactionG rId
    
    let mIns@[_,_]     = extractNode     "mIns" <$> gs
    let (reaction:_)   = extractNode     "reaction" <$> gs
    let (catalyst:_)   = extractNode     "catalyst" <$> gs
    let (accelerate:_) = extractRelation "catalyst" "reaction" <$> gs
    let (mOut:_)       = extractNode     "mOuts" <$> gs
    let (pF:_)         = extractRelation "reaction" "mOuts" <$> gs

    pure (mIns, (catalyst, accelerate), reaction, [(mOut, pF)])
-- можно было матчить mIn1, mIn2 ... раздельно в построении графа запроса, но я подумал что таким путем
-- будет труднее обобщить до переменного числа исходных молекул, например
-- поэтому либо так, либо как выше, с мержом графов в один, где вершины и отношения - мультимапа



-- напишите функцию, которая по двум заданным молекулам ищет путь через реакции и молекулы с наименьшей длиной
getShortestPath :: Molecule -> Molecule -> IO [Path]
getShortestPath mFrom mTo = do
        res <- runQueryDB $ query $ getShortestPathQ (toNode mFrom) (toNode mTo)
        pure $ unpack <$> res
    where
        unpack = fromJust . exactMaybe @Path . flip (!) "p"

        asCypher m = toCypher $ NodeSelector Nothing (labels m) (toList $ getProps m) []

        getShortestPathQ :: Node -> Node -> Text
        getShortestPathQ f t = "match p = shortestPath(" <> asCypher f <> "-[*]-" <> asCypher t <> ") return p"



main :: IO ()
main = do
    clearDB

    putTestData

    -- put/get reaction
    let (mOut : mIns) = [Molecule 700 "test1a" "test1b",
                         Molecule 701 "test2a" "test2b",
                         Molecule 702 "test3a" "test3b"]
    pf <- generate $ arbitrary @PRODUCT_FROM
    let catalyst = Catalyst 703 "test4a" Nothing
    acc <- generate $ arbitrary @ACCELERATE
    let reaction = Reaction 704 "test5a"
    putWholeReaction mIns (catalyst, acc) reaction [(mOut, pf)]
    res <- getWholeReaction 704
    print res

    -- get shortest path
    start <- getMoleculebyId 30
    print start
    end <- getMoleculebyId 80
    print end
    p <- getShortestPath start end
    print p
    --pure ()









-- for demonstration purpose only
getMoleculebyId :: Int -> IO Molecule
getMoleculebyId id = do
    res <- runQueryDB $ getGraph $ getMByIDQ id
    let [resMol] :: [Molecule] = extractNode "m" <$> res
    pure resMol
    where
        getMByIDQ :: Int -> State GraphGetRequest ()
        getMByIDQ id = modify $ addNode "m" $ defaultNodeReturn #
                withLabelQ ''Molecule #
                withProp ("id", I id) #
                withReturn allProps
