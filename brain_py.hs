{-# LANGUAGE QuasiQuotes #-}

module Brain_py where
import Control.Applicative
import Str

before_main = [str|
from sys import stdout, stdin
BYTE = 256

class BuildLambda():
    def __init__(self):
        self.processes = []
    
    def increment(self):
        self.processes.append("increment")
        return self
    
    def decrement(self):
        self.processes.append("decrement")
        return self
        return self

    def next_var(self):
        self.processes.append("next_var")
        return self
    
    def prev_var(self):
        self.processes.append("prev_var")
        return self

    def print_var(self):
        self.processes.append("print_var")
        return self
    
    def get_var(self):
        self.processes.append("get_var")
        return self

    def loop_var(self, builder):
        self.processes.append(["loop_var", builder])
        return self

    def evaluate(self, evaled):
        for p in self.processes:
            if p == "increment":
                evaled.increment()
            elif p == "decrement": 
                evaled.decrement()
            elif p == "next_var":
                evaled.next_var()
            elif p == "prev_var":
                evaled.prev_var()
            elif p == "print_var":
                evaled.print_var()
            elif p == "get_var":
                evaled.get_var()
            elif type(p) is list and p[0] == "loop_var":
                evaled.loop_var(p[1])
        return evaled

class TuringTape():
    def __init__(self):
        self.tape = [ 0 for _ in range(0, 1000)]
        self.head = 0
    
    def increment(self):
        value = self.tape[self.head]
        self.tape[self.head] = (value + 1) % BYTE
        return self

    def decrement(self):
        value = self.tape[self.head]
        self.tape[self.head] = (value - 1 + BYTE) % BYTE
        return self

    def next_var(self):
        self.head += 1
        if len(self.tape) <= self.head:
            self.tape = [ self.tape[i] if len(self.tape) <= i else 0 for i in range(0, len(self.tape) + 1000)]
        return self

    def prev_var(self):
        self.head -= 1
        if self.head < 0:
            self.tape = [ 0 if i < 1000 else self.tape[i] for i in range(0, len(self.tape) + 1000)]
        return self

    def print_var(self):
        stdout.write(chr(self.tape[self.head]))
        return self

    def loop_var(self, builder):
        if self.tape[self.head] == 0:
            return self
        return builder.evaluate(self).loop_var(builder)
    
    def get_var(self):
       self.tape[self.head] = ord(stdin.read(1))
       return self
|]

convert_brainfuck :: String -> String
convert_brainfuck "" = ""
convert_brainfuck ('+':rest) =  ".increment()" ++ convert_brainfuck rest
convert_brainfuck ('-':rest) = ".decrement()" ++ convert_brainfuck rest
convert_brainfuck ('>':rest) = ".next_var()" ++ convert_brainfuck rest
convert_brainfuck ('<':rest) = ".prev_var()" ++ convert_brainfuck rest
convert_brainfuck ('.':rest) = ".print_var()" ++ convert_brainfuck rest
convert_brainfuck (',':rest) = ".get_var()" ++ convert_brainfuck rest
convert_brainfuck ('[':rest) = ".loop_var(BuildLambda()" ++ convert_brainfuck rest
convert_brainfuck (']':rest) = ")" ++ convert_brainfuck rest
convert_brainfuck (_:rest) = convert_brainfuck rest

create_run :: String -> String
create_run input = "TuringTape()" ++ (convert_brainfuck input)

create_program :: String -> String
create_program input = before_main ++ (create_run input)
