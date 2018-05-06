{-# LANGUAGE QuasiQuotes #-}

module Main where
import Control.Applicative
import Str

before_main = [str|
import Data.Char
import Control.Applicative

incl :: [String] -> [String]
incl [(now:rest), next_ptr] = [(chr $ 1 + ord now):rest, next_ptr]

decl :: [String] -> [String]
decl [(now:rest), next_ptr] = [(chr $ (ord now) - 1):rest, next_ptr]

next_var :: [String] -> [String]
next_var [prev_ptr, []] = [('\0':prev_ptr), []]
next_var [prev_ptr, (now:rest)] = [(now:prev_ptr), rest]

prev_var :: [String] -> [String]
prev_var [[], next_ptr] = [['\0'], next_ptr]
prev_var [[only_one], next_ptr] = [['\0'], (only_one:next_ptr)]
prev_var [(now:rest), next_ptr] = [rest, (now:next_ptr)]

puts :: [String] -> IO [String]
puts [prev_ptr, next_ptr] = do
    putStr [head prev_ptr]
    return [prev_ptr, next_ptr] :: IO [String]

gets :: [String] -> IO [String]
gets [prev_ptr, next_ptr] = do
    new_head <- getChar
    return [((:) new_head $ tail prev_ptr), next_ptr] :: IO [String]
|]

first_exp :: String
first_exp = "return [\"hello\", \"world\"] :: IO [String]"

convert_brainfuck :: String -> String
convert_brainfuck "" = first_exp
convert_brainfuck ('+':rest) = "incl <$> (" ++ convert_brainfuck rest ++ ")"
convert_brainfuck ('-':rest) = "decl <$> (" ++ convert_brainfuck rest ++ ")"
convert_brainfuck ('>':rest) = "next_var <$> (" ++ convert_brainfuck rest ++ ")"
convert_brainfuck ('<':rest) = "prev_var <$> (" ++ convert_brainfuck rest ++ ")"
convert_brainfuck ('.':rest) = "puts =<< (" ++ convert_brainfuck rest ++ ")"
convert_brainfuck (',':rest) = "gets =<< (" ++ convert_brainfuck rest ++ ")"
{-
 TODO
convert_brainfuck ('[':rest) = "while(tape[position] != 0){" ++ convert_brainfuck rest 
convert_brainfuck (']':rest) = "}" ++ convert_brainfuck rest
-}

create_run :: String -> String
create_run input = "main = " ++ (convert_brainfuck input)

create_program :: String -> String
create_program input = before_main ++ (create_run input)

brainfuck :: String
--brainfuck = ">+++++++++[<++++++++>-]<.>+++++++[<++++>-]<+.+++++++..+++.[-]>++++++++[<++++>-]<.>+++++++++++[<+++++>-]<.>++++++++[<+++>-]<.+++.------.--------.[-]>++++++++[<++++>-]<+.[-]++++++++++."
brainfuck = ">+++++++++<++++++++>-<.>+++++++<++++>-<+.+++++++..+++.->++++++++<++++>-<.>+++++++++++<+++++>-<.>++++++++<+++>-<.+++.------.--------.->++++++++<++++>-<+.-++++++++++."
main = putStrLn $ create_program brainfuck
