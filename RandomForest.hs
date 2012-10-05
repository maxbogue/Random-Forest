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

generateForestDecisionTree :: Ord a => Attributes -> [Sample a] -> Int -> RVar (DecisionTree a)
generateForestDecisionTree attrs samples m = case nub $ map snd samples of
    []  -> fail "Can't generate a decision tree without any samples."
    [d] -> return $ Decision d
    _   -> if Map.null attrs
        then return $ Decision $ mode $ map snd samples
        else do
            attrPairs <- sample m $ Map.assocs attrs
            let attrs' = Map.fromList attrPairs
            let best = bestAttribute attrs' samples
            subtrees <- mapM
                (\v -> case filterSamples best v samples of
                    []       -> return $ (v, Decision $ mode $ map snd samples)
                    samples' -> do
                        subtree <- generateForestDecisionTree (Map.delete best attrs) samples' m
                        return (v, subtree))
                (attrs Map.! best)
            return $ Tree best $ Map.fromList subtrees

generateForest :: Ord a => Attributes -> [Sample a] -> Int -> Int -> Int -> RVar (RandomForest a)
generateForest attrs samples n m size = replicateM size $ do
    samples' <- choices n samples
    generateForestDecisionTree attrs samples' m

runForest :: Ord a => Choices -> RandomForest a -> a
runForest choices = mode . map (decide choices)
