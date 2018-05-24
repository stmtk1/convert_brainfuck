{-# LANGUAGE QuasiQuotes #-}

module Brain_java where
import Control.Applicative
import Str

before_main = [str|
import java.io.InputStreamReader;
import java.io.IOException;

class Main{
    
    public static void main(String[] args){
        Main m = new Main();
        m.run();
    }

    char[] tape;
    int position;

    Main(){
        tape = new char[1000];
        for(int i = 0; i < tape.length; i++)
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
        System.out.print(tape[position]);
    }

    void next_var(){
        position++;
        if(position >= tape.length){
            char[] new_tape = new char[tape.length + 1000];
            for(int i = 0; i < new_tape.length; i++){
                if(i < tape.length){
                    new_tape[i] = tape[i];
                }else{
                    new_tape[i] = 0;
                }
            }
            tape = new_tape;
        }
    }

    void prev_var(){
        if(position <= 0){
            char[] new_tape = new char[tape.length + 1000];
            for(int i = 0; i < new_tape.length; i++){
                if(i < 1000){
                    new_tape[i] = 0;
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
        InputStreamReader reader = new InputStreamReader(System.in);
        try{
            tape[position] = (char)reader.read();
            reader.close();
        }catch(IOException e){
            e.printStackTrace();
        }

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
convert_brainfuck (_:rest) = convert_brainfuck rest

create_run :: String -> String
create_run input = "void run(){" ++ (convert_brainfuck input) ++ "System.out.println();}}"

create_program :: String -> String
create_program input = before_main ++ (create_run input)

brainfuck :: String
brainfuck = ">+++++++++[<++++++++>-]<.>+++++++[<++++>-]<+.+++++++..+++.[-]>++++++++[<++++>-]<.>+++++++++++[<+++++>-]<.>++++++++[<+++>-]<.+++.------.--------.[-]>++++++++[<++++>-]<+.[-]++++++++++."

--main = putStrLn $ create_program brainfuck
