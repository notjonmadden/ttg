module Persistence(save, load) where
    import Character
    import Data.List.Split
    -- import System.IO.Strict 
    import System.IO(
        Handle,
        IOMode(ReadMode, AppendMode),
        withFile,
        readFile,
        hPutStrLn)
    import System.Directory (doesFileExist)
    
    data Field = Name String | Class String | Level String | Experience String
        deriving (Show)

    applyField :: Character -> Field -> Character
    applyField c f =
        case f of
            Name v -> c { name = v }
            Class v -> 
                case parseClass v of
                    Just cls -> c { cClass = cls }
                    Nothing -> c
            Level v -> c { level = read v }
            Experience v -> c { experience = read v }


    write :: Character -> Handle -> IO ()
    write c h =
        let fWrite = hPutStrLn h in do
            fWrite ("Name=" ++ (name c))
            fWrite ("Class=" ++ (getClassName (cClass c)))
            fWrite ("Level=" ++ (show $ level c))
            fWrite ("Experience=" ++ (show $ experience c))

    serialize :: Character -> String
    serialize c =
        unlines []

    readField :: String -> Field
    readField line = 
        parse line where 
            parse l = case (splitOn "=" l) of
                ["Name", v] -> Name v
                ["Class", v] -> Class v
                ["Level", v] -> Level v
                ["Experience", v] -> Experience v

    readFields :: String -> [Field]
    readFields s =
        map readField (lines s)

    stub :: Character
    stub =
        Character "" Jedi 0 0

    load :: FilePath -> IO Character
    load fp = do
        s <- readFile fp
        let fields = readFields s in
            return $ foldl applyField stub fields
        
    save :: FilePath -> Character -> IO ()
    save fp c =
        let writeCharacter = write c in
            withFile fp AppendMode writeCharacter
