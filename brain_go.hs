
{-# LANGUAGE QuasiQuotes #-}

module Brain_go where
import Control.Applicative
import Str

before_main = [str|
package main

import(
    "fmt"
    //"strings"
)

type TuringTape struct {
    tape []byte
    head int
}

func create() TuringTape {
    ret := TuringTape{}
    ret.tape = make([]byte, 1000)
    for i := 0; i < 1000; i++ {
        ret.tape[i] = 0
    }

    ret.head = 0
    return ret
}

func increment(a TuringTape) TuringTape {
    a.tape[a.head]++
    return a
}

func decrement(a TuringTape) TuringTape {
    a.tape[a.head]--
    return a
}

func next_var(a TuringTape) TuringTape {
    a.head++
    if len(a.tape) <= a.head {
        new_tape := make([]byte, len(a.tape) + 1000)
        for i := 0; i < len(new_tape); i++ {
            if i < len(a.tape) {
                new_tape[i] = a.tape[i]
            }else {
                new_tape[i] = 0
            }
        }
        a.tape = new_tape
    }
    return a
}

func prev_var(a TuringTape) TuringTape {
    a.head--
    if a.head < 0 {
        new_tape := make([]byte, len(a.tape) + 1000)
        for i := 0; i < len(new_tape); i++ {
            if i < 1000 {
                new_tape[i] = 0
            }else {
                new_tape[i] = a.tape[i - 1000]
            }
        }
        a.tape = new_tape
        a.head += 1000
    }
    return a
}

func print_var(a TuringTape) TuringTape {
    fmt.Printf("%c", a.tape[a.head])
    return a
}

func get_var(a TuringTape) TuringTape {
    return a
}

|]

convert_brainfuck :: String -> String
convert_brainfuck "" = ""

convert_brainfuck ('+':rest) = "a = increment(a)\n" ++ convert_brainfuck rest
convert_brainfuck ('-':rest) = "a = decrement(a)\n" ++ convert_brainfuck rest
convert_brainfuck ('>':rest) = "a = next_var(a)\n" ++ convert_brainfuck rest
convert_brainfuck ('<':rest) = "a = prev_var(a)\n" ++ convert_brainfuck rest
convert_brainfuck ('.':rest) = "a = print_var(a)\n" ++ convert_brainfuck rest
-- convert_brainfuck (',':rest) = " get_var()\n" ++ convert_brainfuck rest
convert_brainfuck ('[':rest) = " for a.tape[a.head] != 0 {\n" ++ convert_brainfuck rest
convert_brainfuck (']':rest) = "}\n" ++ convert_brainfuck rest
convert_brainfuck (_:rest) = convert_brainfuck rest

{-
convert_brainfuck ('+':rest) = " increment" ++ convert_brainfuck rest
convert_brainfuck ('-':rest) = " decrement" ++ convert_brainfuck rest
convert_brainfuck ('>':rest) = " next_var" ++ convert_brainfuck rest
convert_brainfuck ('<':rest) = " prev_var" ++ convert_brainfuck rest
convert_brainfuck ('.':rest) = " print_var" ++ convert_brainfuck rest
convert_brainfuck (',':rest) = " get_var" ++ convert_brainfuck rest
convert_brainfuck ('[':rest) = " (loop_func" ++ convert_brainfuck rest
convert_brainfuck (']':rest) = ")" ++ convert_brainfuck rest
-}

create_main :: String -> String
create_main input = "func main(){\nvar a TuringTape = create()\n" ++ (convert_brainfuck input) ++ "}"


create_program :: String -> String
create_program input = before_main ++ (create_main input)

--brainfuck :: String
--brainfuck = ">+++++++++[<++++++++>-]<.>+++++++[<++++>-]<+.+++++++..+++.[-]>++++++++[<++++>-]<.>+++++++++++[<+++++>-]<.>++++++++[<+++>-]<.+++.------.--------.[-]>++++++++[<++++>-]<+.[-]++++++++++."
--brainfuck = ">+++++++++[<.++++++++>-]<>+++++++[<++++>-]<+++++++++++[-]>++++++++[<++++>-]<>+++++++++++[<+++++>-]<>++++++++[<+++>-]<+++--------------[-]>++++++++[<++++>-]<+[-]++++++++++"
--brainfuck = ">+++++++++[<++++++++>-.]<."
--brainfuck = "-.>.<.<.>>>>."

--main = putStrLn $ create_program brainfuck
