module Character (
    Character(..),
    CharacterClass(..),
    createCharacter,
    giveExperience,
    parseClass, getClassName,
    showShort) where

    import Data.Char (toLower)
    import Level

    data CharacterClass = Jedi | Soldier | Scoundrel | Noble | Scout
        deriving (Show)
    
    data Character = Character { 
        name       :: String, 
        cClass     :: CharacterClass,
        level      :: Integer,
        experience :: Integer }
        deriving (Show)

    parseClass :: String -> Maybe CharacterClass
    parseClass s =
        case map toLower s of
            "jedi" -> Just Jedi
            "soldier" -> Just Soldier
            "scoundrel" -> Just Scoundrel
            "noble" -> Just Noble
            "scout" -> Just Scout
            _ -> Nothing
    
    getClassName :: CharacterClass -> String
    getClassName c =
        case c of
            Jedi -> "Jedi"
            Soldier -> "Soldier"
            Scoundrel -> "Scoundrel"
            Noble -> "Noble"
            Scout -> "Scout"

    createCharacter :: String -> CharacterClass -> Character
    createCharacter name cClass =
        Character name cClass 1 0

    giveExperience :: Integer -> Character -> Character
    giveExperience amount char =
        let newAmount = amount + (experience char)
            level = getLevelFromTotalExperience newAmount
        in
            char { experience = newAmount, level = (number level) }

    showShort :: Character -> String
    showShort c =
        (name c) ++ ", a Level " ++ (show $ level c) ++ " " ++ (getClassName $ (cClass c)) ++ " with " ++ (show $ experience c) ++ " experience."