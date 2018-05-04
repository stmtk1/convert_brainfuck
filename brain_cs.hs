{-# LANGUAGE QuasiQuotes #-}

module Main where
import Control.Applicative
import Str

before_main = [str|

using System;

class Brain{
    
    static void Main(){
        Brain b = new Brain();
        b.run();
    }

    char[] tape;
    int position;

    Brain(){
        tape = new char[1000];
        for(int i = 0; i < tape.Length; i++)
            tape[i] = (char)0;
        position = 0;
    }

    void incl(){
        tape[position]++;
    }

    void decl(){
        tape[position]--;
    }

    void puts(){
        Console.Write(tape[position]);
    }

    void next_var(){
        position++;
        if(position >= tape.Length){
            char[] new_tape = new char[tape.Length + 1000];
            for(int i = 0; i < new_tape.Length; i++){
                if(i < tape.Length){
                    new_tape[i] = tape[i];
                }else{
                    new_tape[i] = (char)0;
                }
            }
            tape = new_tape;
        }
    }

    void prev_var(){
        if(position <= 0){
            char[] new_tape = new char[tape.Length + 1000];
            for(int i = 0; i < new_tape.Length; i++){
                if(i < 1000){
                    new_tape[i] = (char)0;
                }else{
                    new_tape[i] = tape[i - 1000];
                }
            }
            tape = new_tape;
            position += 1000;
        }
        position--;
    }

    void gets(){
        tape[position] = (char)Console.Read();
    }
|]

convert_brainfuck :: String -> String
convert_brainfuck "" = ""
convert_brainfuck ('+':rest) = "incl();" ++ convert_brainfuck rest
convert_brainfuck ('-':rest) = "decl();" ++ convert_brainfuck rest
convert_brainfuck ('>':rest) = "next_var();" ++ convert_brainfuck rest
convert_brainfuck ('<':rest) = "prev_var();" ++ convert_brainfuck rest
convert_brainfuck ('.':rest) = "puts();" ++ convert_brainfuck rest
convert_brainfuck (',':rest) = "gets();" ++ convert_brainfuck rest
convert_brainfuck ('[':rest) = "while(tape[position] != 0){" ++ convert_brainfuck rest
convert_brainfuck (']':rest) = "}" ++ convert_brainfuck rest

create_run :: String -> String
create_run input = "void run(){" ++ (convert_brainfuck input) ++ "Console.WriteLine();}}"

create_program :: String -> String
create_program input = before_main ++ (create_run input)

brainfuck :: String
brainfuck = ">+++++++++[<++++++++>-]<.>+++++++[<++++>-]<+.+++++++..+++.[-]>++++++++[<++++>-]<.>+++++++++++[<+++++>-]<.>++++++++[<+++>-]<.+++.------.--------.[-]>++++++++[<++++>-]<+.[-]++++++++++."

main = putStrLn $ create_program brainfuck
