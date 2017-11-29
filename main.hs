import System.Environment
import Character
import Persistence
import Data.Char

saveFolder :: FilePath
saveFolder =
    "./characters"

getSaveFile :: String -> FilePath
getSaveFile charName =
    saveFolder ++ "/" ++ (map toLower charName) ++ ".txt"

enterNewCharacter :: IO (Maybe Character)
enterNewCharacter = do
    putStrLn "Name:"
    name <- getLine
    putStrLn "Class: (Jedi, Soldier, Scout, Scoundrel, or Noble)"
    className <- getLine

    case parseClass className of
        Just c -> return (Just (createCharacter name c))
        Nothing -> return Nothing


main = do
    args <- getArgs
    case args of
        ["create", charName, className] ->
            case parseClass className of
                Just c -> 
                    let char = createCharacter charName c in do
                        save (getSaveFile charName) char
                        putStrLn $ show char
                Nothing -> putStrLn "Bad class"
                
        ["delete", charName] ->
            putStrLn ("loading " ++ charName ++ "...")

        ["see", charName] -> do
            char <- load (getSaveFile charName)
            putStrLn $ showShort char

        ["givex", charName, amount] -> do
            char <- load (getSaveFile charName)
            let mChar = giveExperience (read amount) char in do
                save (getSaveFile charName) char
                putStrLn $ show mChar

        [] -> putStrLn "No command"
        _ -> putStrLn "Bad command"