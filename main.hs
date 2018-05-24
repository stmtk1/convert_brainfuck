import System.Environment
import Brain_cpp
import Brain_rb
import System.Posix.Files 
import System.Process
import Control.Applicative

main :: IO ()
main = do
    (i_file, o_file) <- parseArg <$> getArgs
    bf <- readFile i_file
    writeFile o_file $ convert bf $ get_extension o_file

{-
-- input file is @i_file
-- output file is @o_file
-}

parseArg :: [String] -> (String, String)
parseArg ("-o" : o_file : i_file : []) = (i_file, o_file)
parseArg (i_file : "-o" : o_file : []) = (i_file, o_file)
parseArg (i_file : []) = ("", i_file)

get_extension :: String -> String
get_extension "" = ""
get_extension ('.':answer) = answer
get_extension (_:back) = get_extension back

convert :: String -> String -> String
convert bf "cpp" = Brain_cpp.create_program bf
convert bf "rb" = Brain_rb.create_program bf
