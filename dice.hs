module Dice where
    import System.Random
    
    type Die = Integer
    type Roll = Integer

    roll :: Die -> IO Roll
    roll d =
        getStdRandom $ randomR (1, d)

    rolls :: [Die] -> [IO Roll]
    rolls = 
        map roll