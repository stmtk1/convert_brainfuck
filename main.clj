
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
(-> '((0)) next_var
 increment
 increment
 increment
 increment
 increment
 increment
 increment
 increment
 increment
 (loop_func
 prev_var
 increment
 increment
 increment
 increment
 increment
 increment
 increment
 increment
 next_var
 decrement
)
 prev_var
 print_var
 next_var
 increment
 increment
 increment
 increment
 increment
 increment
 increment
 (loop_func
 prev_var
 increment
 increment
 increment
 increment
 next_var
 decrement
)
 prev_var
 increment
 print_var
 increment
 increment
 increment
 increment
 increment
 increment
 increment
 print_var
 print_var
 increment
 increment
 increment
 print_var
 (loop_func
 decrement
)
 next_var
 increment
 increment
 increment
 increment
 increment
 increment
 increment
 increment
 (loop_func
 prev_var
 increment
 increment
 increment
 increment
 next_var
 decrement
)
 prev_var
 print_var
 next_var
 increment
 increment
 increment
 increment
 increment
 increment
 increment
 increment
 increment
 increment
 increment
 (loop_func
 prev_var
 increment
 increment
 increment
 increment
 increment
 next_var
 decrement
)
 prev_var
 print_var
 next_var
 increment
 increment
 increment
 increment
 increment
 increment
 increment
 increment
 (loop_func
 prev_var
 increment
 increment
 increment
 next_var
 decrement
)
 prev_var
 print_var
 increment
 increment
 increment
 print_var
 decrement
 decrement
 decrement
 decrement
 decrement
 decrement
 print_var
 decrement
 decrement
 decrement
 decrement
 decrement
 decrement
 decrement
 decrement
 print_var
 (loop_func
 decrement
)
 next_var
 increment
 increment
 increment
 increment
 increment
 increment
 increment
 increment
 (loop_func
 prev_var
 increment
 increment
 increment
 increment
 next_var
 decrement
)
 prev_var
 increment
 print_var
 (loop_func
 decrement
)
 increment
 increment
 increment
 increment
 increment
 increment
 increment
 increment
 increment
 increment
 print_var
)
