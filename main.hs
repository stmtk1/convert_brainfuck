import System.Environment
import Brain_cpp
import System.Posix.Files 
--import System.Cmd
import System.Process

main :: IO ()
main = do
    (i_file, o_file) <- parseArg <$> getArgs
    (writeFile o_file) =<< create_program <$> readFile i_file

{-
-- input file is @i_file
-- output file is @o_file
-}

parseArg :: [String] -> (String, String)
parseArg ("-o" : o_file : i_file : []) = (i_file, o_file)
parseArg (i_file : "-o" : o_file : []) = (i_file, o_file)
parseArg (i_file : []) = ("", i_file)

{-
outputFile :: FilePath -> String -> IO ()
outputFile file prog = let
    create_and_write :: Bool -> IO ()
    create_and_write True  = writeFile file prog >> putStrLn "asfas"
    --create_and_write False = touchFile file >> writeFile file prog
    create_and_write False = putStrLn "false" >> rawSystem "touch" [file] -- >> writeFile file prog 
    in create_and_write =<< (fileExist file)

-}
