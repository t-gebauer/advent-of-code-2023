(local {: read-lines : map : printv } (require :lib))
(local V (require :vec3))
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

;; According to https://en.wikipedia.org/wiki/Skew_lines#Nearest_points
(λ nearest-points [line1 line2]
  (let [[p1 d1] line1
        [p2 d2] line2
        n (V.cross d1 d2)
        n1 (V.cross d1 n)
        n2 (V.cross d2 n)
        x1 (/ (V.dot (V.sub p2 p1) n2)
              (V.dot d1 n2))
        x2 (/ (V.dot (V.sub p1 p2) n1)
              (V.dot d2 n1))
        c1 (V.add p1 (V.mul d1 x1))
        c2 (V.add p2 (V.mul d2 x2))]
    [c1 c2 x1 x2]))

(λ line-between [h1 t1 h2 t2]
  (let [[p1 d1] h1
        [p2 d2] h2
        p (V.add p1 (V.mul d1 t1))
        d (V.sub (V.add p2 (V.mul d2 t2))
                 p)]
    [p d]))

(λ find-lower-limits [hailstones]
  (let [count (# hailstones)
        lower-limit (icollect [_ _ (ipairs hailstones)]
                      1)]
    (var new-limit? true)
    (while new-limit?
      (set new-limit? false)
      ;; for each pair
      (for [i 1 count]
        (print i)
        (for [j (+ i 1) count]
          (let [h1 (. hailstones i)
                h2 (. hailstones j)
                l1 (. lower-limit i)
                l2 (. lower-limit j)
                v1 (line-between h1 l1 h2 l2)
                v2 (line-between h1 l1 h2 (+ l2 1))
                v3 (line-between h1 l1 h2 (+ l2 2))
                v4 (line-between h1 (+ l1 1) h2 l2)
                v5 (line-between h1 (+ l1 2) h2 l2)]
            ;; for each other hailstone
            (for [k 1 count]
              (when (and (not= k i)
                         (not= k j))
                (let [h3 (. hailstones k)
                      [_ _ x1 y1] (nearest-points h3 v1)
                      [_ _ x2 y2] (nearest-points h3 v2)
                      [_ _ x3 y3] (nearest-points h3 v3)
                      [_ _ x4 y4] (nearest-points h3 v4)
                      [_ _ x5 y5] (nearest-points h3 v5)]
                  (if
                    (and (> x1 0)
                         (>= x2 x1)
                         (>= x3 x2)
                         (>= x4 x1)
                         (>= x5 x4)
                         (>= y1 0)
                         (>= y2 0)
                         (>= y3 0)
                         (>= y4 0)
                         (>= y5 0)
                         (<= y1 1)
                         (<= y2 1)
                         (<= y3 1)
                         (<= y4 1)
                         (<= y5 1)
                         (> x1 (. lower-limit k)))
                    (do
                      (tset lower-limit k (math.ceil x1)))))))))))
    [hailstones lower-limit]))

(λ find-upper-limits [[hailstones lower-limit]]
  (let [count (# hailstones)
        upper-limit (icollect [_ _ (ipairs hailstones)]
                      1e15)]
    ;; for each pair
    (for [i 1 count]
      (print i)
      (for [j (+ i 1) count]
        (let [h1 (. hailstones i)
              h2 (. hailstones j)
              l1 (. upper-limit i)
              l2 (. upper-limit j)
              v1 (line-between h1 l1 h2 l2)
              v2 (line-between h1 l1 h2 (- l2 1))
              v3 (line-between h1 l1 h2 (- l2 2))
              v4 (line-between h1 (- l1 1) h2 l2)
              v5 (line-between h1 (- l1 2) h2 l2)]
          ;; for each other hailstone
          (for [k 1 count]
            (when (and (not= k i)
                       (not= k j))
              ;; TODO: check > lower-limit ?
              (let [h3 (. hailstones k)
                    [_ _ x1 y1] (nearest-points h3 v1)
                    [_ _ x2 y2] (nearest-points h3 v2)
                    [_ _ x3 y3] (nearest-points h3 v3)
                    [_ _ x4 y4] (nearest-points h3 v4)
                    [_ _ x5 y5] (nearest-points h3 v5)]
                (if
                  (and (>= x1 1)
                       (>= x2 1)
                       (>= x3 1)
                       (>= x4 1)
                       (>= x5 1)
                       (<= x2 x1)
                       (<= x3 x2)
                       (<= x4 x1)
                       (<= x5 x4)
                       (>= y1 0)
                       (>= y2 0)
                       (>= y3 0)
                       (>= y4 0)
                       (>= y5 0)
                       (<= y1 1)
                       (<= y2 1)
                       (<= y3 1)
                       (<= y4 1)
                       (<= y5 1)
                       (< x1 (or (. upper-limit k) 1e300))
                       (>= x1 (. lower-limit k)))
                  (do
                    (tset upper-limit k (math.floor x1))))))))))
    [hailstones lower-limit upper-limit]))

(->> (read-input)
     find-lower-limits
     find-upper-limits
     printv)

;; Both lower and upper limits are correct for the example input. But the upper-limits are far (2e14) from the solution.
;;
;; For the real input the, the upper-limits are lower than the lower-limits...
