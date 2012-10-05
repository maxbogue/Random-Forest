import Control.Monad
import qualified Data.Map as Map
import Data.Random.RVar (runRVar)
import Data.Random.Source.DevRandom

import DecisionTree
import RandomForest

root =
    Tree "Patrons?" $ Map.fromList [
        ("None", Decision False),
        ("Some", Decision True),
        ("Full", Tree "WaitEstimate" $ Map.fromList [
            (">60", Decision False),
            ("30-60", Tree "Alternate?" $ Map.fromList [
                ("No", Tree "Reservation" $ Map.fromList [
                    ("No", Tree "Bar?" $ Map.fromList [
                        ("No", Decision False),
                        ("Yes", Decision True)
                    ]),
                    ("Yes", Decision True)
                ]),
                ("Yes", Tree "Fri/Sat?" $ Map.fromList [
                    ("No", Decision False),
                    ("Yes", Decision True)
                ])
            ]),
            ("10-30", Tree "Hungry?" $ Map.fromList [
                ("No", Decision True),
                ("Yes", Tree "Alternate?" $ Map.fromList [
                    ("No", Decision False),
                    ("Yes", Tree "Raining?" $ Map.fromList [
                        ("No", Decision False),
                        ("Yes", Decision True)
                    ])
                ])
            ]),
            ("0-10", Decision True)
        ])
    ]

main :: IO ()
main = do
    let attrs = getAttributes root
    test_samples <- replicateM 1000 $ generateSample attrs root
    samples <- replicateM 100 $ generateSample attrs root
    let tree = generateDecisionTree attrs samples
    let results = map (\(choices, decision) -> decide choices tree == decision) test_samples
    print $ foldr (\v n -> if v then n else n + 1) 0 results
    forest <- runRVar (generateForest attrs samples 60 5 30) DevURandom
    let results' = map (\(choices, decision) -> runForest choices forest == decision) test_samples
    print $ foldr (\v n -> if v then n else n + 1) 0 results
    return ()
