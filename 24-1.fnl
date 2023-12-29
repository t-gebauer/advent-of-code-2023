(local {: read-lines : map : printv} (require :lib))
(local V (require :vec2))
(local re (require :re))

(local pattern (re.compile "
  line <- {| vec s '@' s vec |}
  vec <- {| num ',' s num ',' s num |}
  num <- {[%d-]+} -> tonumber
  s <- ' '+
" {: tonumber}))

(λ read-input []
  (let [lines (read-lines)
        hailstones (map #(pattern:match $1) lines)]
    hailstones))

;; https://mathworld.wolfram.com/Determinant.html
(λ det [a b c d]
  "determinant"
  (- (* a d)
     (* b c)))

;; https://mathworld.wolfram.com/Line-LineIntersection.html
(λ line-intersection [a b]
  "returns the intersection point or nil when the lines are parallel"
  (let [[a1 ad] a
        [b1 bd] b
        [x1 y1] a1
        [x2 y2] (V.add a1 ad)
        [x3 y3] b1
        [x4 y4] (V.add b1 bd)
        d (det (- x1 x2) (- y1 y2)
               (- x3 x4) (- y3 y4))
        k1 (det x1 y1
                x2 y2)
        k2 (det x3 y3
                x4 y4)]
    (if (not= d 0)
      (V.div [(det k1 (- x1 x2)
                   k2 (- x3 x4))
              (det k1 (- y1 y2)
                   k2 (- y3 y4))]
             d))))

(λ sign [x]
  (if
    (< x 0) -1
    (> x 0) 1
    0))

(λ is-future-x? [line x]
  (let [[[px _py] [dx _dy]] line]
    (= (sign dx)
       (sign (- x px)))))

(λ count-intersections [hailstones]
  (var count 0)
  (let [hailstones (if (< (# hailstones) 10)
                     hailstones
                     ;; Necessary to improve precission :(
                     (map (fn [[p d]]
                            [(map #(/ $1 10000000000000) p) d]) hailstones))
        [testmin testmax] (if (< (# hailstones) 10)
                            [7 27]
                            ;; [200000000000000 400000000000000]
                            [20 40])]
    (for [i 1 (# hailstones)]
      (local a (. hailstones i))
      (for [j (+ i 1) (# hailstones)]
        (local b (. hailstones j))
        (case (line-intersection a b)
          [ix iy] (do
                    (if (and (>= ix testmin)
                             (>= iy testmin)
                             (<= ix testmax)
                             (<= iy testmax))
                      (if (and (is-future-x? a ix)
                               (is-future-x? b ix))
                        (set count (+ count 1)))))))))
  count)

(->> (read-input)
     count-intersections
     printv)
