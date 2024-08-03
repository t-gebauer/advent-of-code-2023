(local {: read-lines : map : printv : gcd} (require :lib))
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


(λ project [n p]
  "n: normal vector of the plane (needs to be normalized)
   p: a vector to project"
  (V.sub p (V.mul n (V.dot p n))))

(λ project-all [hailstones n]
  (let [projected (icollect [_ [p d] (ipairs hailstones)]
                    [(project n p)
                     (project n d)])]
    projected))

(λ intersections [lines distance]
  (local count (or lines.count (# lines)))
  (var p nil)
  (var res nil)
  (let [i 1]
    (for [j (+ i 1) count]
      (let [a (. lines i)
            b (. lines j)
            [c1 c2 x1 x2] (nearest-points a b)]
        (if
          ;; parallel?
          (not= x1 x1) ; NaN is never equal
          (do
            nil) ; ignore
          true ; in 2d they will always intersect if not parallel (ignoring rounding errors)
          (if
            ;; first intersection point
            (= nil p)
            (do
              (set p c1)
              (set res [i j x1 x2]))
            ;; all other intersection points must be close to the first
            (< (V.length (V.sub p c1)) distance)
            (do ; great, continue
              nil)
            ;; else: abort
            (do
              (lua "return false")))
          ;; else: abort
          (do
            (printv "??? cannot happen, lines must intersect?"
                    c1 c2 x1 x2 (V.length (V.sub c2 c1)) "-" (V.length (V.sub c1 (or p [0 0 0]))))
            (lua "return false"))))
      nil))
  ; (printv p)
  (if (= nil p)
    false
    res))

;; Caching seems to be slower than just computing all values (2x)
(λ memo-table [fun tbl]
  (local cache {})
  (local nil* {}) ; unique nil identifier
  (setmetatable {:count (# tbl)} ;; the __len metamethod does not (always) exit in luajit
                {:__index (fn [_self i]
                              (case (or (. cache i)
                                        (do (tset cache i (case (. tbl i)
                                                            val (fun val)
                                                            nil nil*))
                                          (. cache i)))
                                (where (= nil*)) nil
                                val val))}))

(local gcd (fn [a b]
             (math.abs (gcd a b))))


;; Search for the direction vector
;; Imagine the rock throw as a line throw all hailstones
;; If all hailstones (lines) were to be orthogonally projected onto the
;; direction vector of the rock, then by discarding the parallel part
;; we can map them onto a plane, which has the direction as normal vector.
;; In this plane, all lines must intersect, exactly at one point, where
;; the rock will hit them all.
;; As always, for small values we can brute-force a solution.
(λ find-direction [hailstones]
  (let [intersection-distance (case (# hailstones)
                                5 1e-10
                                300 1e13)
        min -99
        max +99]
    (for [x 0 max] ; negative x matches would just be mirrored
      (for [y min max]
        (for [z min max]
          (if (and (not (and (= x 0)
                             (= y 0)
                             (= z 0)))
                   (or (= 1 (gcd x y))
                       (= 1 (gcd y z))
                       (= 1 (gcd x z))))
            (let [n [x y z]
                  n* (V.norm n)
                  projected (project-all hailstones n*)
                  ; project-one (fn [[p d]] [(project n* p) (project n* d)])
                  ; projected (memo-table project-one hailstones)
                  ints (intersections projected intersection-distance)]
              (if ints
                (printv "found" n ints)))))))))

; "found"  [-999 -1 1]
; "found"  [-999 3 -3]
;; ... lots of such cases where one dimension is high and both others are low (and inverse of each other)
;; "found" [x y z]     [h1 h2 x1 x2 (distance c1 c2)
;; "found" [2 -2 -953] [67 252 -1562030707823.5 539437247855.13 0]
;; "found" [2 -2 -791] [27 144 -7358540964203.1 -12459647441462 0]
;; "found" [2 -2 -695] [21 128 602545183198.5 1516984422077.5 0]
;; "found" [2 -2 -693] [64 154 720002665068.97 4132973291391.6 0]

(->> (read-input)
     find-direction
     printv)
