;;; Version a: Now with a proper grammar definition?

(local is-part-1 (= nil (. arg 1)))

(local grammar (let [{: P : V : R : C : Ct} (require :lpeg)]
                 (P {1 (* "Game " (C (V :number)) ":" (^ (* (V :set) ";") 0) (V :set) -1)
                     :set (Ct (* (^ (* (V :color) ",") 0) (V :color)))
                     :color (Ct (* " " (C (V :number)) " " (C (V :name))))
                     :name (+ (P "red") "green" "blue")
                     :number (^ (R "09") 1)
                     })))

(print (accumulate [acc 0
                    line (io.lines)]
         (let [[game & sets] [(grammar:match line)]
               max {}]
           (each [_ set_ (ipairs sets)]
             (each [_ [num color] (ipairs set_)]
               (tset max color (math.max num (or (. max color) 0)))))
           (+ acc
              (if is-part-1
                ;; part 1
                (if (and (<= (. max :red) 12)
                         (<= (. max :green) 13)
                         (<= (. max :blue) 14))
                  game
                  0)
                ;; part 2
                (* (. max :red)
                   (. max :green)
                   (. max :blue)))))))
