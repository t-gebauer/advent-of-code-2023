;;; Someone on reddit pointed out, that we can completly ignore the time component:
;;; - For each possible direction
;;; - Subtract the direction from each hailstone's direction
;;; - If all new lines intersect, than this point is the starting point for our rock throw
;;; Finds the solution in less than a minute.

(local {: read-lines : map : printv} (require :lib))
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

(λ abs [n]
  (if (< n 0) (- n) n))

(λ inc [n]
  (* -1 (if (> n 0)
          n
          (- n 1))))

(λ all-directions []
  "... where no component is ever 0"
  (var x 0)
  (var y 0)
  (var z 0)
  (λ []
    (let [max (abs x)]
      (if
        (= z y (- max)) (do
                          (set z 1)
                          (set y 1)
                          (set x (inc x))
                          (if (> x 0) (printv x))) ;; display some progress

        (= z (- max)) (do
                        (set z 1)
                        (set y (inc y)))
        (set z (inc z))))
    [x y z]))

(λ all-intersect [hailstones dir]
  (var result nil)
  (var abort? false)
  (for [i 1 (- (# hailstones) 1) &until abort?]
    (let [[p1 d1] (. hailstones i)
          [p2 d2] (. hailstones (+ i 1))
          d1 (V.sub d1 dir)
          d2 (V.sub d2 dir)
          [c1 c2] (nearest-points [p1 d1] [p2 d2])]
      (set result c1)
      (if (> (V.length (V.sub c1 c2))
             (if (. hailstones 6) 1 0)) ;; some leeway for rounding errors (but not for the example)
        (set abort? true))))
  (if abort? nil result))

(λ find-direction [hailstones]
  (var result nil)
  (each [dir (all-directions) &until result]
    (case (all-intersect hailstones dir)
      p (do
          (printv dir p)
          (set result (+ (. p 1) (. p 2) (. p 3))))))
  result)

(->> (read-input)
     find-direction
     (string.format "%0.f")
     print)
