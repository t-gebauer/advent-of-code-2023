(local pattern
       (let [{: P : C : Ct : R} (require :lpeg)
             spaces (^ (P " ") 0)
             number (^ (R "09") 1)
             numbers (Ct (^ (* spaces (C number)) 1))]
         (* "Card" spaces number ":" numbers spaces "|" numbers)))

(λ count-matches [list other]
  (let [t {}]
    (each [_ num (ipairs list)]
      (tset t num true))
    (accumulate [count 0
                 _ num (ipairs other)]
      (+ count (if (. t num) 1 0)))))

(print
 ;; First, count matching numbers on each line
 (let [match-counts (icollect [line (io.lines)]
                      (let [[win my] [(pattern:match line)]]
                        (count-matches win my)))
       accumulated-counts {}]
   ;; Then, accumulate from the bottom up
   (for [i (# match-counts) 1 -1]
     (let [match-count (. match-counts i)
           cards (faccumulate [n 0 j (+ i 1) (+ i match-count) &until (> j (# match-counts))]
                   (+ n (. accumulated-counts j)))]
       (tset accumulated-counts i (+ 1 cards))))
   ;; Finally, sum them all up
   (accumulate [sum 0
                _ n (ipairs accumulated-counts)]
     (+ sum n))))
