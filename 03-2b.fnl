;;; Version b: Same as version a, except accumulations occurs directly during
;;; parsing.

(fn acc-gear-ratio [acc num pos]
  (case (. acc.positions pos)
    existing (set acc.sum (+ acc.sum (* existing num)))
    nil (tset acc.positions pos num))
  acc)

(λ make-pattern [width]
  (let [{: P : R : B : C : Cc : Cp : V} (require :lpeg)
        digit (R "09")
        gear (P "*")
        number (^ digit 1)
        ;; It is not possible to produce captures inside forward (#) or backward (B) *and predicates*,
        ;; therefore, we must separately record the current position and an rule dependent offset.
        digit-with-gear (/ (* (+ (* (Cc +1) digit (# gear)) ; right
                                 (* (Cc -1) (B gear) digit) ; left
                                 (* (Cc (+ width +1)) digit (# (* (P (- width 0)) gear))) ; down-right
                                 (* (Cc (+ width +0)) digit (# (* (P (- width 1)) gear))) ; down
                                 (* (Cc (+ width -1)) digit (# (* (P (- width 2)) gear))) ; down-left
                                 (* (Cc (- (+ width +1))) (B (* gear (P (- width 0)))) digit) ; top-left
                                 (* (Cc (- (+ width +0))) (B (* gear (P (- width 1)))) digit) ; top
                                 (* (Cc (- (+ width -1))) (B (* gear (P (- width 2)))) digit) ; top-right
                                 ) (Cp))
                              #(+ $1 $2))
        number-with-gear (P [(+ (* digit-with-gear (^ digit 0))
                                        (* digit (V 1)))])]
    (* (Cc {:positions {} :sum 0}) (^ (+ (% (C number-with-gear) acc-gear-ratio) number (P 1)) 0))))

(let [lines (icollect [line (io.lines)] line)
      width (# (. lines 1))
      pattern (make-pattern width)
      result (pattern:match (table.concat lines))]
  (print result.sum))
