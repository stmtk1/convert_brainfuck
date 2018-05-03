{-# LANGUAGE QuasiQuotes #-}

module Main where
import Control.Applicative
import Str

before_main = [str|
import java.util.LinkedList;
import java.util.ListIterator;
import java.io.InputStreamReader;
import java.io.IOException;

class Main{
    
    public static void main(String[] args){
        Main m = new Main();
        m.run();
    }

    LinkedList<Character> tape;
    ListIterator<Character> position;
    char value;

    Main(){
        tape = new LinkedList<>();
        tape.addLast((char)0);
        position = tape.listIterator();
        position.next();
        value = (char)0;
    }

    void incl(){
        position.set(++value);
    }

    void decl(){
        position.set(--value);
    }

    void puts(){
        System.out.println((int)value);
    }

    void next_var(){
        if(!position.hasNext()){
            tape.addLast((char)0);
        }
        value = position.next();
    }

    void prev_var(){
        if(!position.hasPrevious()){
            tape.addFirst((char)0);
        }else{
            value = position.previous();
        }
    }

    void gets(){
        InputStreamReader reader = new InputStreamReader(System.in);
        try{
            position.set(value = (char)reader.read());
            reader.close();
        }catch(IOException e){
            e.printStackTrace();
        }
    }|]

{-
convert_brainfuck :: String -> String
convert_brainfuck "" = ""
convert_brainfuck ('+':rest) = "incl();\n" ++ convert_brainfuck rest
convert_brainfuck ('-':rest) = "decl();\n" ++ convert_brainfuck rest
convert_brainfuck ('>':rest) = "next_var();\n" ++ convert_brainfuck rest
convert_brainfuck ('<':rest) = "prev_var();\n" ++ convert_brainfuck rest
convert_brainfuck ('.':rest) = "puts();\n" ++ convert_brainfuck rest
convert_brainfuck (',':rest) = "gets();\n" ++ convert_brainfuck rest
convert_brainfuck ('[':rest) = "while(value != 0){\n" ++ convert_brainfuck rest
convert_brainfuck (']':rest) = "}\n" ++ convert_brainfuck rest
-}
convert_brainfuck :: String -> String
convert_brainfuck "" = ""
convert_brainfuck ('+':rest) = "incl();" ++ convert_brainfuck rest
convert_brainfuck ('-':rest) = "decl();" ++ convert_brainfuck rest
convert_brainfuck ('>':rest) = "next_var();" ++ convert_brainfuck rest
convert_brainfuck ('<':rest) = "prev_var();" ++ convert_brainfuck rest
convert_brainfuck ('.':rest) = "puts();" ++ convert_brainfuck rest
convert_brainfuck (',':rest) = "gets();" ++ convert_brainfuck rest
convert_brainfuck ('[':rest) = "while(value != 0){" ++ convert_brainfuck rest
convert_brainfuck (']':rest) = "}" ++ convert_brainfuck rest


create_run :: String -> String
create_run input = "void run(){" ++ (convert_brainfuck input) ++ "System.out.println();}}"

create_program :: String -> String
create_program input = before_main ++ (create_run input)

brainfuck :: String
--brainfuck = ">+++++++++[<++++++++>-]<.>+++++++[<++++>-]<+.+++++++..+++.[-]>++++++++[<++++>-]<.>+++++++++++[<+++++>-]<.>++++++++[<+++>-]<.+++.------.--------.[-]>++++++++[<++++>-]<+.[-]++++++++++."
--brainfuck = ">+++++++++[<++++++++>-]<.>+++++++[<++++>-]"
brainfuck = "++++++++++++++++++++++.>>"

main = putStrLn $ create_program brainfuck
