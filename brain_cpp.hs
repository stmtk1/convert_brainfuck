{-# LANGUAGE QuasiQuotes #-}

module Brain_cpp where
import Control.Applicative
import Str

before_main = [str|
#include <iostream>
#include <string>
#include <list>

using namespace std;

list<char> tape;
list<char>::iterator position;

void incl(){
    (*position)++;
}

void decl(){
    (*position)--;
}

void next_var(){
    if(next(position) == tape.end()){
        tape.push_back(0);
    }
    position++;
}

void prev_var(){
    if(position == tape.begin()){
        tape.push_front(0);
    }
    position--;
}

void puts(){
    cout << *position;
}

void gets(){
    cin >> *position;
} |]

convert_brainfuck :: String -> String
convert_brainfuck "" = ""
convert_brainfuck ('+':rest) = "incl();\n" ++ convert_brainfuck rest
convert_brainfuck ('-':rest) = "decl();\n" ++ convert_brainfuck rest
convert_brainfuck ('>':rest) = "next_var();\n" ++ convert_brainfuck rest
convert_brainfuck ('<':rest) = "prev_var();\n" ++ convert_brainfuck rest
convert_brainfuck ('.':rest) = "puts();\n" ++ convert_brainfuck rest
convert_brainfuck (',':rest) = "gets();\n" ++ convert_brainfuck rest
convert_brainfuck ('[':rest) = "while(*position != 0){\n" ++ convert_brainfuck rest
convert_brainfuck (']':rest) = "}\n" ++ convert_brainfuck rest
convert_brainfuck (_:rest) = convert_brainfuck rest

create_main :: String -> String
create_main input = "int main(){tape.push_back(0);\nposition = tape.begin();\n" ++ (convert_brainfuck input) ++ "cout << endl;return 0;}"

create_program :: String -> String
create_program input = before_main ++ (create_main input)

--brainfuck :: String
--brainfuck = ">+++++++++[<++++++++>-]<.>+++++++[<++++>-]<+.+++++++..+++.[-]>++++++++[<++++>-]<.>+++++++++++[<+++++>-]<.>++++++++[<+++>-]<.+++.------.--------.[-]>++++++++[<++++>-]<+.[-]++++++++++."
--brainfuck = ">+++++++++[<++++++++>-]<."

--main = putStrLn $ create_program brainfuck
--

