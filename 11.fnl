(local {: read-lines : map : printv : chars} (require :lib))
(local grid2d (require :grid2d))
(local V (require :vec2))

(local is-part-1 (= :1 (or (. arg 1) :1)))

(λ dist [a b]
  (let [[dx dy] (V.sub b a)]
    (+ (math.abs dx)
       (math.abs dy))))

(λ empty-array [l initial]
  (local result [])
  (for [i 1 l]
    (tset result i initial))
  result)

(λ find-empty-spaces [grid stars]
  (let [cols (empty-array grid.width true)
        rows (empty-array grid.height true)]
    (each [_ [sx sy] (ipairs stars)]
      (tset cols sx false)
      (tset rows sy false))
    (let [cols (icollect [i v (ipairs cols)]
                 (if v i))
          rows (icollect [i v (ipairs rows)]
                 (if v i))]
      (values cols rows))))

(let [lines (->> (read-lines)
                 (map chars))
      grid (grid2d lines)
      stars (grid:find-all :#)
      (empty-columns empty-rows) (find-empty-spaces grid stars)
      space-size (if is-part-1 2 1_000_000)
      space-size (- space-size 1)]

  (λ space-adjust [[ax ay] [bx by]]
    (var [x y] [bx by])
    (each [_ col (ipairs empty-columns)]
      (if
        (< ax col bx) (set x (+ x space-size))
        (< bx col ax) (set x (- x space-size))))
    (each [_ row (ipairs empty-rows)]
      (if
        (< ay row by) (set y (+ y space-size))
        (< by row ay) (set y (- y space-size))))
    [x y])

  (var sum 0)
  (for [i 1 (- (# stars) 1)]
    (for [j (+ i 1) (# stars)]
      (let [a (. stars i)
            b (. stars j)
            c (space-adjust a b)]
        (set sum (+ sum (dist a c))))))
  (printv sum))
