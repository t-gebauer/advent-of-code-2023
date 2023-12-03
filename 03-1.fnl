(λ make-pattern [width]
  (let [{: P : R : B : C : V} (require :lpeg)
        digit (R "09")
        symbol (- (P 1) (+ digit "."))
        digit-with-symbol (+ (* digit (# symbol)) ; right
                             (* digit (# (* (P width) symbol))) ; down-right
                             (* digit (# (* (P (- width 1)) symbol))) ; down
                             (* digit (# (* (P (- width 2)) symbol))) ; down-left
                             (* (B symbol) digit) ; left
                             (* (B (* symbol (P width))) digit) ; top-left
                             (* (B (* symbol (P (- width 1)))) digit) ; top
                             (* (B (* symbol (P (- width 2)))) digit) ; top-right
                             )
        number (^ digit 1)
        ;; A number with at least one digit with an adjacent symbol
        number-with-symbol (P [(+ (* digit-with-symbol (^ digit 0))
                                  (* digit (V 1)))])]
    (^ (+ (C number-with-symbol) number (P 1)) 0)))

(let [lines (icollect [line (io.lines)] line)
      ;; Pad each line with dots, to ensure that no number is at an edge
      width (+ (length (. lines 1)) 2)
      text (accumulate [acc ""
                        _ line (ipairs lines)]
             (.. acc "." line "."))
      pattern (make-pattern width)
      matches [(pattern:match text)]
      sum (accumulate [sum 0
                       _ num (ipairs matches)]
            (+ sum (tonumber num)))]
  (print sum))
