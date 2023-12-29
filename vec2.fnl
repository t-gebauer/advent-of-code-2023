(λ add [[ax ay] [bx by]]
  [(+ ax bx)
   (+ ay by)])

(λ sub [[ax ay] [bx by]]
  [(- ax bx)
   (- ay by)])

(λ mul [[x y] s]
  [(* s x)
   (* s y)])

(λ div [[x y] s]
  [(/ x s)
   (/ y s)])

(λ equal? [[ax ay] [bx by]]
  (and (= ax bx)
       (= ay by)))

(λ zero? [[ax ay]]
  (= ax ay 0))

(λ turn-left [[x y]]
  [y (- x)])

(λ turn-right [[x y]]
  [(- y) x])

(λ abs [[x y]]
  (+ (math.abs x)
     (math.abs y)))

(let [north [0 -1]
      south [0 1]
      east [1 0]
      west [-1 0]]
  {: add
   : sub
   : mul
   : div
   : equal?
   : zero?
   : north
   : south
   : east
   : west
   :up north
   :down south
   :left west
   :right east
   : turn-left
   : turn-right
   : abs})
