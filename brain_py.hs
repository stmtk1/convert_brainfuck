{-# LANGUAGE QuasiQuotes #-}

module Brain_py where
import Control.Applicative
import Str

before_main = [str|
from sys import stdout
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
                evaled = evaled.increment()
            elif p == "decrement": 
                evaled = evaled.decrement()
            elif p == "next_var":
                evaled = evaled.next_var()
            elif p == "prev_var":
                evaled = evaled.prev_var()
            elif p == "print_var":
                evaled = evaled.print_var()
            elif p == "get_var":
                evaled = evaled.get_var()
            elif type(p) is list and p[0] == "loop_var":
                evaled = evaled.loop_var(builder)
        return evaled

class LinkedList():
    def __init__(self):
        self.value = 0
        self.next = None
        self.prev = None

    def next_var(self):
        if self.next == None:
            self.next = LinkedList()
            self.next.prev = self
        return self.next

    def prev_var(self):
        if self.prev == None:
            self.prev = LinkedList()
            self.prev.next = self
        return self.prev

    def increment(self):
        self.value = (self.value + 1) % BYTE
        return self

    def decrement(self):
        self.value = (self.value - 1 + BYTE) % BYTE
        return self

    def print_var(self):
        stdout.write(chr(self.value))
        return self

    def loop_var(self, builder):
        if self.value == 0:
            return self
        return builder.evaluate(self).loop_var(builder)
|]

convert_brainfuck :: String -> String
convert_brainfuck "" = ""
convert_brainfuck ('+':rest) =  ".increment()" ++ convert_brainfuck rest
convert_brainfuck ('-':rest) = ".decrement()" ++ convert_brainfuck rest
convert_brainfuck ('>':rest) = ".next_var()" ++ convert_brainfuck rest
convert_brainfuck ('<':rest) = ".prev_var()" ++ convert_brainfuck rest
convert_brainfuck ('.':rest) = ".print_var()" ++ convert_brainfuck rest
-- TODO
--convert_brainfuck (',':rest) = ".get_value()" ++ convert_brainfuck rest
convert_brainfuck ('[':rest) = ".loop_var(BuildLambda()" ++ convert_brainfuck rest
convert_brainfuck (']':rest) = ")" ++ convert_brainfuck rest
convert_brainfuck (_:rest) = convert_brainfuck rest

create_run :: String -> String
create_run input = "LinkedList()" ++ (convert_brainfuck input)

create_program :: String -> String
create_program input = before_main ++ (create_run input)
