(local pattern
       (let [{: P : C : Ct : R} (require :lpeg)
             spaces (^ (P " ") 0)
             number (^ (R "09") 1)]
         (* "Card" spaces number ":"
            (Ct "")
            (^ (* spaces (% (C number) (fn [acc num]
                                         (tset acc num true)
                                         acc))) 1)
            spaces "|" spaces
            (^ (* spaces (% (C number) (fn [acc num]
                                         (case (. acc num)
                                           true (set acc.res (if acc.res
                                                               (* acc.res 2)
                                                               1)))
                                         acc))) 1))))

(print (accumulate [acc 0
                    line (io.lines)]
         (let [matches (pattern:match line)]
           (+ acc (or matches.res 0)))))
