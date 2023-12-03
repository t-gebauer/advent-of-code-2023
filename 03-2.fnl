;;; This works only for smaller inputs. LPeg has a backtracking limit of 255
;;; chars. But this pattern backtracks more than twice the width (140) of the
;;; input.

(λ make-pattern [width]
  (let [{: P : R : B : C : Cc : Ct : V} (require :lpeg)
        digit (R "09")
        gear (P "*")
        number (^ digit 1)
        digit-with-gear-forward ;; TODO: generate these rules?
        (+ (* (B gear) digit (# (* (P (- width 1)) digit))) ; gear left, digit down-right
           (* (B gear) digit (# (* (P (- width 2)) digit))) ; gear left, digit down
           (* (B gear) digit (# (* (P (- width 3)) digit))) ; gear left, digit down-left
           (* digit (# (* gear digit))) ; gear right, digit right
           (* digit (# (* gear (P width) digit))) ; gear right, digit down-right
           (* digit (# (* gear (P (- width 1)) digit))) ; gear right, digit down
           (* digit (# (* gear (P (- width 2)) digit))) ; gear right, digit down-left
           (* digit (# (* (P width) gear (P width) digit))) ; gear down-right, digit down-right
           (* digit (# (* (P width) gear (P (- width 1)) digit))) ; gear down-right, digit down
           (* digit (# (* (P width) gear (P (- width 2)) digit))) ; gear down-right, digit down-left
           (* digit (# (* (P (- width 1)) gear (P width) digit))) ; gear down, digit down-right
           (* digit (# (* (P (- width 1)) gear (P (- width 1)) digit))) ; gear down, digit down
           (* digit (# (* (P (- width 1)) gear (P (- width 2)) digit))) ; gear down, digit down-left
           (* digit (# (* (P (- width 2)) gear (P width) digit))) ; gear down-left, digit down-right
           (* digit (# (* (P (- width 2)) gear (P (- width 1)) digit))) ; gear down-left, digit down
           (* digit (# (* (P (- width 2)) gear (P (- width 2)) digit))) ; gear down-left, digit down-left
           )
        digit-with-gear-backward
        (+ (* (B (* digit (P (- width 1)))) digit (# gear)) ; gear right, digit up-left
           (* (B (* digit (P (- width 2)))) digit (# gear)) ; gear right, digit up
           (* (B (* digit (P (- width 3)))) digit (# gear)) ; gear right, digit up-right
           (* (B (* digit gear)) digit) ; gear left, digit left
           (* (B (* digit (P width) gear)) digit) ; gear left, digit up-left
           (* (B (* digit (P (- width 1)) gear)) digit) ; gear left, digit up
           (* (B (* digit (P (- width 2)) gear)) digit) ; gear left, digit up-right
           (* (B (* digit (P (- width 0)) gear (P width))) digit) ; gear up-left, digit up-left
           (* (B (* digit (P (- width 1)) gear (P width))) digit) ; gear up-left, digit up
           (* (B (* digit (P (- width 2)) gear (P width))) digit) ; gear up-left, digit up-right
           (* (B (* digit (P (- width 0)) gear (P (- width 1)))) digit) ; gear up, digit up-left
           (* (B (* digit (P (- width 1)) gear (P (- width 1)))) digit) ; gear up, digit up
           (* (B (* digit (P (- width 2)) gear (P (- width 1)))) digit) ; gear up, digit up-right
           (* (B (* digit (P (- width 0)) gear (P (- width 2)))) digit) ; gear up-right, digit up-left
           (* (B (* digit (P (- width 1)) gear (P (- width 2)))) digit) ; gear up-right, digit up
           (* (B (* digit (P (- width 2)) gear (P (- width 2)))) digit) ; gear up-right, digit up-right
           )
        number-with-gear-forward (P [(+ (* digit-with-gear-forward (^ digit 0))
                                        (* digit (V 1)))])
        number-with-gear-backward (P [(+ (* digit-with-gear-backward (^ digit 0))
                                         (* digit (V 1)))])]
    (^ (+ (Ct (* (Cc :f) (C number-with-gear-forward)))
          (Ct (* (Cc :b) (C number-with-gear-backward)))
           number (P 1)) 0)))

(λ get-gear-ratios [matches]
  (let [stack []
        ratios []]
    (each [_ [dir num] (ipairs matches)]
      (case dir
        :f (table.insert stack num)
        :b (table.insert ratios (* num (table.remove stack)))))
    ratios))

(let [lines (icollect [line (io.lines)] line)
      ;; Pad each line with dots, to ensure that no number is at an edge
      width (+ (length (. lines 1)) 4)
      text (accumulate [acc ""
                        _ line (ipairs lines)]
             (.. acc ".." line ".."))
      pattern (make-pattern width)
      matches [(pattern:match text)]
      gear-ratios (get-gear-ratios matches)
      sum (accumulate [sum 0
                       _ num (ipairs gear-ratios)]
            (+ sum (tonumber num)))]
  (print sum))
