(λ add [[ax ay] [bx by]]
  [(+ ax bx)
   (+ ay by)])

(λ sub [[ax ay] [bx by]]
  [(- ax bx)
   (- ay by)])

(λ equal? [[ax ay] [bx by]]
  (and (= ax bx)
       (= ay by)))

(λ zero? [[ax ay]]
  (= ax ay 0))

{: add
 : sub
 : equal?
 : zero?
 :north [0 -1]
 :south [0 1]
 :east [1 0]
 :west [-1 0]}
