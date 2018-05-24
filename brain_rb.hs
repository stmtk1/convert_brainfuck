{-# LANGUAGE QuasiQuotes #-}

module Brain_rb where
import Control.Applicative
import Str

before_main = [str|
class BuildLambda
    def initialize
        @funcs = []
    end

    def increment
        @funcs.push(:increment)
        self
    end

    def decrement
        @funcs.push(:decrement)
        self
    end

    def next_var
        @funcs.push(:next_var)
        self
    end

    def prev_var
        @funcs.push(:prev_var)
        self
    end

    def print_value
        @funcs.push(:print_value)
        self
    end

    def get_value
        @funcs.push(:get_value)
        self
    end

    def loop_self(builder)
        @funcs.push([:loop_self, builder])
        self
    end

    def evaluate lst
        for func in @funcs
            if func == :increment 
                lst = lst.increment
            elsif func == :decrement
                lst = lst.decrement
            elsif func == :next_var then
                lst = lst.next_var
            elsif func == :prev_var then
                lst = lst.prev_var
            elsif func == :print_value then
                lst = lst.print_value
            elsif func == :get_value then
                lst = lst.get_value
            elsif func.class == Array and func[0] == :loop_self then
                lst = lst.loop_self(func[1])
            end
        end
        lst
    end

    def loop_var lst
        return lst if lst.value == 0
        loop_var(evaluate(lst))
    end
end

class TuringHead
    BYTE = 256
    attr_accessor :prev, :next, :value
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

    def loop_self(builder)
        builder.loop_var self
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
convert_brainfuck ('[':rest) = ".loop_self(BuildLambda.new" ++ convert_brainfuck rest
convert_brainfuck (']':rest) = ")" ++ convert_brainfuck rest
convert_brainfuck (_:rest) = convert_brainfuck rest

create_run :: String -> String
create_run input = "TuringHead.new" ++ (convert_brainfuck input)

create_program :: String -> String
create_program input = before_main ++ (create_run input)

brainfuck :: String
brainfuck = ">+++++++++[<++++++++>-]<.>+++++++[<++++>-]<+.+++++++..+++.[-]>++++++++[<++++>-]<.>+++++++++++[<+++++>-]<.>++++++++[<+++>-]<.+++.------.--------.[-]>++++++++[<++++>-]<+.[-]++++++++++."

-- main = putStrLn $ create_program brainfuck
