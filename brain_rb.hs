{-# LANGUAGE QuasiQuotes #-}

module Main where
import Control.Applicative
import Str

before_main = [str|
class TuringHead
    BYTE = 256
    attr_accessor :prev, :next
    def initialize(prev = nil)
        @value = 0
    end

    def increment
        @value = (@value + 1) % BYTE
        self
    end

    def decrement
        @value = (@value + BYTE - 1) % BYTE
        self
    end

    def next_var
        unless @next 
            @next = TuringHead.new
            @next.prev = self
        end
        @next
    end

    def prev_var
        unless @prev
            @prev = TuringHead.new
            @prev.next = self
        end
        @prev
    end

    def print_value
        print @value.chr
        self
    end

    def get_value
        @value = getc.ord
        self
    end

    def loop_self(&block)
        return self if @value == 0
        block.call(self).loop_self(&block)
    end
end
|]

convert_brainfuck :: String -> String
convert_brainfuck "" = ""
convert_brainfuck ('+':rest) =  ".increment" ++ convert_brainfuck rest
convert_brainfuck ('-':rest) = ".decrement" ++ convert_brainfuck rest
convert_brainfuck ('>':rest) = ".next_var" ++ convert_brainfuck rest
convert_brainfuck ('<':rest) = ".prev_var" ++ convert_brainfuck rest
convert_brainfuck ('.':rest) = ".print_value" ++ convert_brainfuck rest
convert_brainfuck (',':rest) = ".get_value" ++ convert_brainfuck rest
convert_brainfuck ('[':rest) = ".loop_self{|a| a" ++ convert_brainfuck rest
convert_brainfuck (']':rest) = "}" ++ convert_brainfuck rest

create_run :: String -> String
create_run input = "TuringHead.new" ++ (convert_brainfuck input)

create_program :: String -> String
create_program input = before_main ++ (create_run input)

brainfuck :: String
brainfuck = ">+++++++++[<++++++++>-]<.>+++++++[<++++>-]<+.+++++++..+++.[-]>++++++++[<++++>-]<.>+++++++++++[<+++++>-]<.>++++++++[<+++>-]<.+++.------.--------.[-]>++++++++[<++++>-]<+.[-]++++++++++."

main = putStrLn $ create_program brainfuck
