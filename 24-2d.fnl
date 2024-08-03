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

(λ shuffle [tbl]
  (let [l (# tbl)]
    (for [i 1 l]
      (let [tmp (. tbl i)
            o (math.random l)]
        (tset tbl i (. tbl o))
        (tset tbl o tmp)))))


(λ dist-line-point [line point]
  (let [[A u] line]
    (/ (V.length (V.cross (V.sub point A)
                          u))
       (V.length u))))

(λ find-perfect-rock-position3 [hailstones]
  (shuffle hailstones)
  (shuffle hailstones)
  (let [[p1 d1] (. hailstones 1)
        [p2 d2] (. hailstones 2)
        max-search 1e+13]

    (λ total-distance [t1 t2]
      (var distsum 0)
      (var perfect-hits 0)
      (var distgrid 0)
      (var timedist 0)
      (let [to (- t2 t1)
            v1 (V.add p1 (V.mul d1 t1))
            v2 (V.add p2 (V.mul d2 t2))
            vecd* (V.sub v2 v1)
            vecd (V.div vecd* to)
            vecp (V.add v1 (V.mul vecd (- t1)))
            vec [vecp vecd]]
        (let [possible-hits (accumulate [sum 2
                                         i hail (ipairs hailstones)]
                              (if (< i 3)
                                sum
                                (+ sum
                                  (let [[c1 c2 x1 x2] (nearest-points vec hail)]
                                    (set timedist (+ (math.abs (- x2 x1))))
                                    (if (and (= x1 (math.floor x1))
                                             (= x2 (math.floor x2)))
                                      (set perfect-hits (+ perfect-hits 1)))
                                    (if (and (<= x1 0)
                                             (<= x2 0))
                                      (set distsum (+ distsum (V.length (V.sub (. vec 1)
                                                                               (. hail 1)))))
                                      (and (<= x1 0)
                                           (> x2 0))
                                      (set distsum (+ distsum (dist-line-point hail vecp)))

                                      (and (<= x2 0)
                                           (> x1 0))
                                      (set distsum (+ distsum (dist-line-point vec (. hail 1)))))

                                    (if
                                      (and (> x1 0)
                                           (> x2 0))
                                      (do
                                        (set distsum (+ distsum (V.length (V.sub c2 c1))))
                                        (set distgrid (+ distgrid (+ (math.min (- x1 (math.floor x1))
                                                                               (- (math.ceil x1) x1))
                                                                     (math.min (- x2 (math.floor x2))
                                                                               (- (math.ceil x2) x2)))))
                                        1)
                                      0)))))]
          (when (= perfect-hits (- (# hailstones) 2))
            (printv "done?" vec)
            (os.exit))
          (values perfect-hits possible-hits distsum distgrid timedist))))

    (var t1 2e+10)
    (var t2 1e+10)
    (var (bestperfect besthits bestdist bestgrid besttimedist) (total-distance t1 t2))
    (while true
      (let [options [[(+ t1 1) t2]
                     [(- t1 1) t2]
                     [t1 (+ t2 1)]
                     [t1 (- t2 1)]
                     [(+ t1 (math.random -100 100)) t2]
                     [t1 (+ t2 (math.random -100 100))]
                     [(+ t1 (math.random -1e+6 1e+6))
                      (+ t2 (math.random -1e+6 1e+6))]
                     [(+ t1 (math.random -1e+8 1e+8))
                      (+ t2 (math.random -1e+8 1e+8))]
                     [(+ t1 (math.random -1e+10 1e+10))
                      (+ t2 (math.random -1e+10 1e+10))]
                     [(* t1 1.1) t2]
                     [(* t1 0.9) t2]
                     [t1 (* t2 1.1)]
                     [t1 (* t2 0.9)]
                     [t2 t1]
                     [(* 0.5 t1) (* 1.5 t2)]
                     [(* 1.5 t1) (* 0.5 t2)]]
            options (icollect [_ [a b] (ipairs options)]
                      [(math.max 1 (math.min max-search (math.floor a)))
                       (math.max 1 (math.min max-search (math.floor b)))])
            options (icollect [_ [a b] (ipairs options)]
                      (if (= a b)
                        [(+ a 1) b]
                        [a b]))]
        (var best nil)
        (each [_ [t1* t2* &as opt] (ipairs options)]
          (let [(perfect hits dist grid timedist) (total-distance t1* t2*)]
            (if (or
                  ; (< timedist besttimedist)
                  (< dist bestdist))
                  ; (and
                  ;   (< dist bestdist)
                  ;   (>= hits (* 0.7 besthits)))
                  ; (and
                  ;   (> hits besthits)
                  ;   (< dist (* 1.3 bestdist))))
                  ; (> hits besthits)
                  ; (and
                  ;   (< dist bestdist)
                  ;   (>= hits (* 0.7 besthits))))
              (do
                (set best opt)
                (set besthits hits)
                (set bestdist dist)
                (set bestperfect perfect)
                (set bestgrid grid)
                (set besttimedist timedist)))))
        (case best
          [t1* t2*] (do
                      (set t1 t1*)
                      (set t2 t2*)
                      (print "best" t1 t2 "=" besthits bestdist bestperfect bestgrid besttimedist)
                      (if (< bestdist 1e+5)
                        (let [v1 (V.add p1 (V.mul d1 t1))
                              v2 (V.add p2 (V.mul d2 t2))
                              vecd* (V.sub v2 v1)
                              vecd (V.div vecd* (- t2 t1))
                              vecp (V.add v1 (V.mul vecd (- t1)))
                              vec [vecp vecd]]
                          (printv v1 v2)
                          (printv "d*" vecd*)
                          (printv vec)))))))
    nil))


(->> (read-input)
     find-perfect-rock-position3
     printv)


; Gets stuck on local minimum?
; best  178381243424  564132261724  = 300 2.5098954156781 137 0.024818420410156

