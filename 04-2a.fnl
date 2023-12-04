;; reuseable functions

(λ read-lines []
  (icollect [line (io.lines)]
    line))

(λ map [fun list]
  (icollect [_ item (ipairs list)]
    (fun item)))

(λ sum [list]
  (accumulate [sum 0 _ n (ipairs list)]
    (+ sum n)))

(λ intersection [list other]
  (let [t {}]
    (each [_ item (ipairs list)]
      (tset t item item))
    (icollect [_ item (ipairs other)]
      (. t item))))

;; puzzle specific logic

(local pattern
       (let [{: P : C : Ct : R} (require :lpeg)
             spaces (^ (P " ") 0)
             number (^ (R "09") 1)
             numbers (Ct (^ (* spaces (C number)) 1))]
         (* "Card" spaces number ":" numbers spaces "|" numbers)))

(λ count-matches-on-line [line]
  (let [[win my] [(pattern:match line)]]
    (length (intersection win my))))

(λ accumulate-bottom-up [numbers]
  (let [accumulated {}]
    (for [i (# numbers) 1 -1]
      (let [num (. numbers i)
            acc (faccumulate [n 0 j (+ i 1) (+ i num) &until (> j (# numbers))]
                  (+ n (. accumulated j)))]
        (tset accumulated i (+ 1 acc))))
    accumulated))

(print (->> (read-lines)
            (map count-matches-on-line)
            accumulate-bottom-up
            sum))
