(local {: P : R : C : Ct} (require :lpeg))

(local is-part-1 (= nil (. arg 1)))

;; ahhh, so ugly ... but it works
(local pattern
       (let [number (^ (R "09") 1)]
         (* "Game "
            (C number)
            ":"
            (^ (+ ";"
                  (Ct (^ (+ ","
                            (Ct (* " "
                                   (C number)
                                   " "
                                   (C (+ (P "red")
                                         "green"
                                         "blue")))))
                         1)))
               1)
            -1 ; failsafe: no characters left
            )))

(print (accumulate [acc 0
                    line (io.lines)]
         (let [[game & sets] [(pattern:match line)]
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
