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
            delete (getSaveFile charName)

        ["see", charName] -> do
            char <- load (getSaveFile charName)
            putStrLn $ showShort char

        ["givex", charName, amount] -> do
            char <- load (getSaveFile charName)
            let mChar = giveExperience (read amount) char in do
                save (getSaveFile charName) mChar
                putStrLn $ show mChar

        [] -> putStrLn "No command"
        _ -> putStrLn "Bad command"