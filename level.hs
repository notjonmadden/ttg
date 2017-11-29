module Level (
    levels,
    number,
    getLevelFromTotalExperience) where

    import Data.List

    data Level = Level { 
        number :: Integer,
        experienceThreshold :: Integer,
        grantsNewFeat :: Bool,
        grantsAbilityIncreases :: Bool }
        deriving (Show)

    getExperienceThreshold :: Integer -> Integer
    getExperienceThreshold 1 = 0
    getExperienceThreshold l =
        getExperienceThreshold (l - 1) + (1000 * (l - 1))

    levelGrantsNewFeat :: Integer -> Bool
    levelGrantsNewFeat 1 = True
    levelGrantsNewFeat l =
        mod l 3 == 0

    levelGrantsNewAbilities :: Integer -> Bool
    levelGrantsNewAbilities l =
        mod l 4 == 0

    levels :: [Level]
    levels = [Level i (getExperienceThreshold i) (levelGrantsNewFeat i) (levelGrantsNewAbilities i) | i <- [1 .. 20]]

    exceedsThreshold :: Integer -> Level -> Bool
    exceedsThreshold e l =
        le > e where
            le = experienceThreshold l

    getLevelFromTotalExperience :: Integer -> Level
    getLevelFromTotalExperience e =
        let first = head levels
            rest = tail levels 
        in
            getLevel rest first where
                getLevel ls prev =
                    case ls of
                        (l:rest) | exceedsThreshold e l -> prev
                        (l:rest) -> getLevel rest l
                        [] -> prev

    getLevel :: Integer -> Maybe Level
    getLevel i =
        find match levels where
            match l = (number l) == i