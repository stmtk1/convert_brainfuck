{-# LANGUAGE QuasiQuotes #-}

module Brain_clj where
import Control.Applicative
import Str

before_main = [str|
(def BYTE 256)

(defn increment [a]
  (let
      [prev_ptr (first a)
      next_ptr (rest a)]
    
    (conj next_ptr (-> 
      prev_ptr
      rest
      (conj (-> prev_ptr first (+ 1) (mod BYTE)))
    ))
   ))

(defn decrement [a]
  (let
      [prev_ptr (first a)
      next_ptr (rest a)]
    
    (conj next_ptr (-> 
      prev_ptr
      rest
      (conj (-> prev_ptr first (+ 255) (mod BYTE)))
    ))
   ))

(defn next_var [a]
  (let 
      [prev_ptr (first a)
       next_ptr (rest a)]
    (if (empty? next_ptr)
      (conj '() (conj prev_ptr 0))
      (->> next_ptr first (conj prev_ptr) (conj (rest next_ptr)))
      )
    ))

(defn prev_var [a]
  (let 
      [prev_ptr (first a)
       next_ptr (rest a)]
    (if (empty? (rest prev_ptr))
      (conj (conj next_ptr (first prev_ptr)) '(0))
      (conj (conj next_ptr (first prev_ptr)) (rest prev_ptr))
      )
    ))

(defn print_var [a]
  (do 
    (-> a first first char print)
    a))

(defn get_var [a]
  (let [prev_ptr (first a)
        next_head (mod (Integer/parseInt (read-line)) BYTE)
        next_ptr (rest a)
        ]
    (conj next_ptr (-> a
        rest
        (conj next_head)
        ))
    ))

(defn exe_func [first_cond & all_funcs]
  (loop
        [value first_cond
         funcs all_funcs]
    (if 
      (empty? funcs)
      value
      (recur ((first funcs) value) (rest funcs))
      )))

(defn loop_func [first_cond & funcs]
  (loop 
      [value first_cond]
         
  (if 
    (->> value first first (= 0))
    value
    (->> value (conj funcs) (apply exe_func) recur)
    )))
|]

convert_brainfuck :: String -> String
convert_brainfuck "" = ""

convert_brainfuck ('+':rest) = " increment\n" ++ convert_brainfuck rest
convert_brainfuck ('-':rest) = " decrement\n" ++ convert_brainfuck rest
convert_brainfuck ('>':rest) = " next_var\n" ++ convert_brainfuck rest
convert_brainfuck ('<':rest) = " prev_var\n" ++ convert_brainfuck rest
convert_brainfuck ('.':rest) = " print_var\n" ++ convert_brainfuck rest
convert_brainfuck (',':rest) = " get_var\n" ++ convert_brainfuck rest
convert_brainfuck ('[':rest) = " (loop_func\n" ++ convert_brainfuck rest
convert_brainfuck (']':rest) = ")\n" ++ convert_brainfuck rest

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
create_main input = "(-> '((0))" ++ (convert_brainfuck input) ++ ")"


create_program :: String -> String
create_program input = before_main ++ (create_main input)

brainfuck :: String
brainfuck = ">+++++++++[<++++++++>-]<.>+++++++[<++++>-]<+.+++++++..+++.[-]>++++++++[<++++>-]<.>+++++++++++[<+++++>-]<.>++++++++[<+++>-]<.+++.------.--------.[-]>++++++++[<++++>-]<+.[-]++++++++++."
--brainfuck = ">+++++++++[<.++++++++>-]<>+++++++[<++++>-]<+++++++++++[-]>++++++++[<++++>-]<>+++++++++++[<+++++>-]<>++++++++[<+++>-]<+++--------------[-]>++++++++[<++++>-]<+[-]++++++++++"
--brainfuck = ">+++++++++[<++++++++>-.]<."
--brainfuck = "-.>.<.<.>>>>."

main = putStrLn $ create_program brainfuck
