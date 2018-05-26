{-# LANGUAGE QuasiQuotes #-}

module Brain_php where
import Control.Applicative
import Str

before_main = [str|
$appended = array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

$tape = array(0);
$index = 0;

define("BYTE", 256);

function increment(){
    global $tape, $index;
    $tape[$index] = ($tape[$index] + 1) % constant("BYTE");
}

function decrement(){
    global $tape, $index;
    $tape[$index] = ($tape[$index] - 1 + constant("BYTE")) % constant("BYTE");
}

function next_var(){
    global $tape, $index, $appended;
    $index++;
    if(count($tape) <= $index)
        $tape = $tape + $appended;
}

function prev_var(){
    global $tape, $index, $appended;
    $index--;
    if($index < 0){
        $tape = $appended + $tape;
        $index += count($appended);
    }
}

function print_var(){
    global $tape, $index;
    print(chr($tape[$index]));
}
|]

convert_brainfuck :: String -> String
convert_brainfuck "" = ""


convert_brainfuck ('+':rest) = " increment();" ++ convert_brainfuck rest
convert_brainfuck ('-':rest) = " decrement();" ++ convert_brainfuck rest
convert_brainfuck ('>':rest) = " next_var();" ++ convert_brainfuck rest
convert_brainfuck ('<':rest) = " prev_var();" ++ convert_brainfuck rest
convert_brainfuck ('.':rest) = " print_var();" ++ convert_brainfuck rest
convert_brainfuck (',':rest) = " get_var();" ++ convert_brainfuck rest
convert_brainfuck ('[':rest) = " while($tape[$index] != 0){" ++ convert_brainfuck rest
convert_brainfuck (']':rest) = "}" ++ convert_brainfuck rest
convert_brainfuck (_:rest) = convert_brainfuck rest

--create_main :: String -> String
--create_main input = "<?php\n" ++ (convert_brainfuck input) ++ "\n?>"


create_program :: String -> String
create_program input = "<?php\n" ++ before_main ++ (convert_brainfuck input) ++ "\n?>"

--brainfuck :: String
--brainfuck = ">+++++++++[<++++++++>-]<.>+++++++[<++++>-]<+.+++++++..+++.[-]>++++++++[<++++>-]<.>+++++++++++[<+++++>-]<.>++++++++[<+++>-]<.+++.------.--------.[-]>++++++++[<++++>-]<+.[-]++++++++++."
--brainfuck = ">+++++++++[<.++++++++>-]<>+++++++[<++++>-]<+++++++++++[-]>++++++++[<++++>-]<>+++++++++++[<+++++>-]<>++++++++[<+++>-]<+++--------------[-]>++++++++[<++++>-]<+[-]++++++++++"
--brainfuck = ">+++++++++[<++++++++>-.]<."
--brainfuck = "-.>.<.<.>>>>."

--main = putStrLn $ create_program brainfuck
