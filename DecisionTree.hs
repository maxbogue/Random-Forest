module DecisionTree where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (nub)
import Data.Traversable (traverse)
import Data.Tuple (swap)
import GHC.Exts (groupWith)
import System.Random

data DecisionTree a
    = Decision a
    | Tree String (Map String (DecisionTree a))
    deriving (Show, Eq)

type Attribute = String

type Attributes = Map Attribute [String]

type Choices = Map Attribute String

type Sample a = (Choices, a)

getAttributes :: DecisionTree a -> Attributes
getAttributes (Decision _) = Map.empty
getAttributes (Tree attr children) = foldl
    Map.union
    (Map.singleton attr (Map.keys children))
    (map getAttributes (Map.elems children))

decide :: DecisionTree a -> Choices -> a
decide (Decision a) _ = a
decide (Tree attr children) choices = decide (children Map.! (choices Map.! attr)) choices

randomElement :: [t] -> IO t
randomElement ls = do
    i <- randomRIO (0, length ls - 1)
    return $ ls !! i

generateSample :: Attributes -> DecisionTree a -> IO (Choices, a)
generateSample attrs tree = do
    (choices, decision) <- walk tree
    missing <- traverse randomElement $ Map.difference attrs choices
    return (Map.union choices missing, decision)
  where
    walk :: DecisionTree a -> IO (Choices, a)
    walk (Decision d) = return (Map.empty, d)
    walk (Tree attr children) = do
        (val, child) <- randomElement $ Map.assocs children
        (choices, decision) <- walk child
        return (Map.insert attr val choices, decision)

entropy :: (Ord a, Floating b) => [Sample a] -> b
entropy = entropy' . distribution where
    distribution samples = let total = fromIntegral $ length samples in
        map ((/ total) . fromIntegral . length) $ groupWith snd samples
    entropy' [] = 0.0
    entropy' (x:xs) = -x * logBase 2 x + entropy' xs

filterSamples attr value = filter (\s -> fst s Map.! attr == value)

bestAttribute :: Ord a => Attributes -> [Sample a] -> Attribute
bestAttribute attrs samples = snd $ maximum $ map (\(a, vs) -> (informationGain a vs, a)) (Map.assocs attrs) where
    numSamples = fromIntegral $ length samples
    totalH = entropy samples
    informationGain attr values = totalH - priorH values where
        priorH [] = 0
        priorH (v:vs) = let samples' = filterSamples attr v samples in
            (fromIntegral $ length samples') / numSamples * entropy samples' + priorH vs

generateDecisionTree attrs samples = case nub $ map snd samples of
    []  -> error "Can't generate a decision tree without any samples."
    [d] -> Decision d
    _   -> if Map.null attrs
        then Decision $ mode $ map snd samples
        else let best = bestAttribute attrs samples in
            Tree best $ Map.fromList [(v, generateSubtree best v) | v <- attrs Map.! best]
  where
    generateSubtree best v = case filterSamples best v samples of
        []       -> Decision $ mode $ map snd samples
        samples' -> generateDecisionTree (Map.delete best attrs) samples'
    mode [] = error "An empty list doesn't have a mode."
    mode ls = snd $ maximum $ map swap $ Map.assocs $ mode' ls where
        mode' [] = Map.empty
        mode' (k:ks) = Map.alter inc k $ mode' ks
        inc Nothing = Just 0
        inc (Just n) = Just $ n + 1
