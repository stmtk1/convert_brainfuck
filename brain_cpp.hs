{-# LANGUAGE QuasiQuotes #-}

module Brain_cpp where
import Control.Applicative
import Str

before_main = [str|
#include <iostream>

using namespace std;

int len;
unique_ptr<char> tape;
char *head;

void init(){
    len = 1000;
    tape = unique_ptr<char>(new char[len]);
    char *new_tape = tape.get();
    head = &new_tape[0];
    for(int i = 0; i < len; i++)
        new_tape[i] = 0;
}

void increment(){
    (*head)++;
}

void decrement(){
    (*head)--;
}

void next_var(){
    char *tape_inner = tape.get();
    head++;
    if(&tape_inner[len] <= head){
        len += 1000;
        char *new_tape = new char[len];
        for(int i = 0; i < len; i++){
            new_tape[i] = i < len - 1000 ? tape_inner[i] : 0;
        }
        tape = unique_ptr<char>(new_tape);
    }
}

void prev_var(){
    char *tape_inner = tape.get();
    head--;
    if(*head < 0){
        len += 1000;
        head += 1000;
        char *new_tape = new char[len];
        for(int i = 0; i < len; i++){
            new_tape[i] = i < 1000 ? 0 : tape_inner[i];
        }
        tape = unique_ptr<char>(new_tape);
    }
}

void print_var(){
    cout << *head;
}

void get_var(){
    cin >> *head;
}
|]

convert_brainfuck :: String -> String
convert_brainfuck "" = ""
convert_brainfuck ('+':rest) = "increment();\n" ++ convert_brainfuck rest
convert_brainfuck ('-':rest) = "decrement();\n" ++ convert_brainfuck rest
convert_brainfuck ('>':rest) = "next_var();\n" ++ convert_brainfuck rest
convert_brainfuck ('<':rest) = "prev_var();\n" ++ convert_brainfuck rest
convert_brainfuck ('.':rest) = "print_var();\n" ++ convert_brainfuck rest
convert_brainfuck (',':rest) = "get_var();\n" ++ convert_brainfuck rest
convert_brainfuck ('[':rest) = "while(*head != 0){\n" ++ convert_brainfuck rest
convert_brainfuck (']':rest) = "}\n" ++ convert_brainfuck rest
convert_brainfuck (_:rest) = convert_brainfuck rest

create_main :: String -> String
create_main input = "int main(){init();\n" ++ (convert_brainfuck input) ++ "return 0;}"

create_program :: String -> String
create_program input = before_main ++ (create_main input)

--brainfuck :: String
--brainfuck = ">+++++++++[<++++++++>-]<.>+++++++[<++++>-]<+.+++++++..+++.[-]>++++++++[<++++>-]<.>+++++++++++[<+++++>-]<.>++++++++[<+++>-]<.+++.------.--------.[-]>++++++++[<++++>-]<+.[-]++++++++++."
--brainfuck = ">+++++++++[<++++++++>-]<."

--main = putStrLn $ create_program brainfuck
--

