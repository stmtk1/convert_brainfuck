import System.Environment
import System.Posix.Files 
import System.Process
import Control.Applicative

-- local files
import Brain_cpp
import Brain_rb
import Brain_cs
import Brain_py
import Brain_java

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
convert bf "cs" = Brain_cs.create_program bf
--convert bf "py" = Brain_py.create_program bf
convert bf "java" = Brain_java.create_program bf
