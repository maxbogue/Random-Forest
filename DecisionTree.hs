import Data.Map (Map, fromList)

data DecisionTree d = Decision d | Tree {
    attr :: String,
    children :: Map String (DecisionTree d)
  } deriving (Show, Eq)

test =
    Tree "Patrons?" $ fromList [
        ("None", Decision False),
        ("Some", Decision True),
        ("Full", Tree "WaitEstimate" $ fromList [
            (">60", Decision False),
            ("30-60", Tree "Alternate?" $ fromList [
                ("No", Tree "Reservation" $ fromList [
                    ("No", Tree "Bar?" $ fromList [
                        ("No", Decision False),
                        ("Yes", Decision True)
                    ]),
                    ("Yes", Decision True)
                ]),
                ("Yes", Tree "Fri/Sat?" $ fromList [
                    ("No", Decision False),
                    ("Yes", Decision True)
                ])
            ]),
            ("10-30", Tree "Hungry?" $ fromList [
                ("No", Decision True),
                ("Yes", Tree "Alternate?" $ fromList [
                    ("No", Decision False),
                    ("Yes", Tree "Raining?" $ fromList [
                        ("No", Decision False),
                        ("Yes", Decision True)
                    ])
                ])
            ]),
            ("0-10", Decision True)
        ])
    ]

main = do
    return ()
