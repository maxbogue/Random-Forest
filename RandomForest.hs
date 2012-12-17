module RandomForest where

import Control.Monad
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (nub)
import Data.Traversable (traverse)
import Data.Tuple (swap)
import GHC.Exts (groupWith)
import Data.Random.Extras (sample, choices)
import Data.Random.RVar (RVar)

import DecisionTree

type RandomForest a = [DecisionTree a]

makeDecisionTreeM :: Ord a => Attributes -> [Sample a] -> Int -> RVar (DecisionTree a)
makeDecisionTreeM attrs samples m = case nub $ map snd samples of
    []  -> fail "Can't gen a decision tree without any samples."
    [d] -> return $ Decision d
    _   -> if Map.null attrs
        then return $ Decision $ mode $ map snd samples
        else do
            attrPairs <- sample m $ Map.assocs attrs
            let attrs' = Map.fromList attrPairs
            let best = bestAttribute attrs' samples
            subtrees <- forM (attrs Map.! best) $ \v ->
                case filterSamples best v samples of
                    []       -> return $ (v, Decision $ mode $ map snd samples)
                    samples' -> do
                        subtree <- makeDecisionTreeM (Map.delete best attrs) samples' m
                        return (v, subtree)
            return $ Tree best $ Map.fromList subtrees

makeRandomForest' :: Ord a => Attributes -> [Sample a] -> Int -> Int -> Int -> RVar (RandomForest a)
makeRandomForest' attrs samples size n m = replicateM size $ do
    samples' <- choices n samples
    makeDecisionTreeM attrs samples' m

makeRandomForest attrs samples size = makeRandomForest' attrs samples size
    (length samples)
    (floor $ logBase 2 (fromIntegral $ Map.size attrs) + 1)

runForest :: Ord a => Choices -> RandomForest a -> a
runForest choices = mode . map (decide choices)
