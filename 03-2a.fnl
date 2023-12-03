;;; Version a: Records the positions of possible gears, and then combines those
;;; numbers with equal position.

(λ make-pattern [width]
  (let [{: P : R : B : C : Cc : Cp : Ct : V} (require :lpeg)
        digit (R "09")
        gear (P "*")
        number (^ digit 1)
        ;; It is not possible to produce captures inside forward (#) or backward (B) *and predicates*,
        ;; therefore, we must separately record the current position and an rule dependent offset.
        digit-with-gear (+ (* (Cp) (Cc 1) digit (# gear)) ; right
                           (* (Cp) (Cc (+ width 1)) digit (# (* (P width) gear))) ; down-right
                           (* (Cp) (Cc (+ width 0)) digit (# (* (P (- width 1)) gear))) ; down
                           (* (Cp) (Cc (+ width -1)) digit (# (* (P (- width 2)) gear))) ; down-left
                           (* (Cp) (Cc -1) (B gear) digit) ; left
                           (* (Cp) (Cc (- (+ width 1))) (B (* gear (P width))) digit) ; top-left
                           (* (Cp) (Cc (- (+ width 0))) (B (* gear (P (- width 1)))) digit) ; top
                           (* (Cp) (Cc (- (+ width -1))) (B (* gear (P (- width 2)))) digit) ; top-right
                           )
        number-with-gear (P [(+ (* digit-with-gear (^ digit 0))
                                        (* digit (V 1)))])]
    (^ (+ (Ct (C number-with-gear)) number (P 1)) 0)))

(λ get-gear-ratios [matches]
  (let [positions {}]
    (each [_ [num pos offset] (ipairs matches)]
      (let [gear-pos (+ pos offset)
            existing (. positions gear-pos)]
        (if existing
          (table.insert existing num)
          (tset positions gear-pos [num]))))
    (let [ratios []]
      (each [pos nums (pairs positions)]
        (case (# nums)
          0 (error "no numbers") ; impossible
          1 nil ; ignore
          2 (table.insert ratios (* (. nums 1) (. nums 2)))
          _ (error (.. "too many numbers at " pos)) ; possible, but does not happen in my input
          ))
    ratios)))

(let [lines (icollect [line (io.lines)] line)
      ;; Pad each line with dots, to ensure that no number is at an edge
      ;; Does not seem to be necessary for my input, but just to be safe.
      width (+ (length (. lines 1)) 2)
      text (accumulate [acc ""
                        _ line (ipairs lines)]
             (.. acc "." line "."))
      pattern (make-pattern width)
      matches [(pattern:match text)]
      gear-ratios (get-gear-ratios matches)
      sum (accumulate [sum 0
                       _ num (ipairs gear-ratios)]
            (+ sum (tonumber num)))]
  (print sum))
