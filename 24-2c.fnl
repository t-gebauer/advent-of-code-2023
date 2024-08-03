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

(λ nearest-distance2 [linea lineb]
  (let [[ap ad] linea
        [bp bd] lineb]
    (λ dist-at [t]
      (let [av (V.add ap (V.mul ad t))
            bv (V.add bp (V.mul bd t))
            d (V.length (V.sub bv av))]
        d))
    (var near 1)
    (var far 1e+13) ; must not be too large, or risk the result to become inrepresentable by lua
    (var dnear (dist-at near))
    (var dfar (dist-at far))
    (var closest (math.min dnear dfar))
    (while (> (- far near) 0)
      (let [test (+ near (math.floor (/ (- far near) 2)))
            test* (+ test 1)
            dtest (dist-at test)
            dtest* (dist-at test*)]

        ;; Check for precission of large numbers. If t is too large.
        (if
          (and (< dnear dtest)
               (< dtest* dtest))
          (print "warn: dtest > dnear, dtest*" dtest dnear dtest*
                 "at" (string.format "%.0f" test) (string.format "%.0f" near) (string.format "%.0f" test*))

          (and (< dfar dtest*)
               (< dtest dtest*))
          (print "warn: dtest* > dfar, dtest" dtest* dfar dtest "at" test* far test))

        (if
          (and (< dnear dtest)
               (< dnear dtest*))
          (do
            (set far test)
            (set dfar dtest)
            (set closest dnear))

          (and (< dfar dtest)
               (< dfar dtest*))
          (do
            (set near test*)
            (set dnear dtest*)
            (set closest dfar))

          (or
            (< dtest dtest*)
            (and (= dtest dtest*)
                 (< dnear dtest)))
          (do
            (set far test)
            (set dfar dtest)
            (set closest dtest))

          ; else
          (do
            (set near test*)
            (set dnear dtest*)
            (set closest dtest*)))))
    closest))

;; ----------------------------------------------------------------------------------------------------

(λ print-interesting-numbers [hailstones]
  ;; only the x-dimension
  (let [p1 (. hailstones 2 1 1)
        d1 (. hailstones 2 2 1)
        p2 (. hailstones 3 1 1)
        d2 (. hailstones 3 2 1)]
    (for [t1 20 23]
      (print t1 "-----------------")
      (let [d0 (- (+ p2 (* d2 t1))
                  (+ p1 (* d1 t1)))]
        (print (string.format "%0.f" d0) (- d2 d1) "-" d1 d2)
        (for [t2 (math.max 1 (- t1 50)) (+ t1 50)]
          (if (= t1 t2)
            (print t2 "---")
            (let [v1 (+ p1 (* d1 t1))
                  v2 (+ p2 (* d2 t2))
                  dx (- v2 v1)
                  tx (- t2 t1)]
              (print t2 (* (- d2 d1) tx) dx tx (% dx tx) (if (= 0 (% dx tx)) "-" "")
                     d0 (% d0 tx))))))))
            ;; ----------------------------------
            ;; Conclusion:
            ;; if (% d0 tx) is 0 then tx is valid
            ;; ----------------------------------
  nil)

(λ dividers [n]
  (let [n (math.abs n)
        dividers []]
    ;; Find the first half of dividers
    (for [i 1 (math.sqrt n)]
      (if (= 0 (% n i))
        (table.insert dividers i)))
    ;; Generate the second half
    ;; But, do not list the highest divider twice
    (let [high (# dividers)
          high (if (= n (^ (. dividers high) 2))
                 (- high 1)
                 high)]
      (for [i high 1 -1]
        (table.insert dividers (/ n (. dividers i)))))
    dividers))


(λ possible-time-offsets [[fp fd] [sp sd] t1]
  (let [fv (V.add fp (V.mul fd t1))
        sv (V.add sp (V.mul sd t1))
        [dx dy dz] (V.sub sv fv)
        divs (or (if (not= 0 dx) (dividers dx))
                 (if (not= 0 dy) (dividers dy))
                 (if (not= 0 dz) (dividers dz)))
        ts []]
    (each [_ t (ipairs divs)]
      (when (and (= 0 (% dx t))
                 (= 0 (% dy t))
                 (= 0 (% dz t)))
        (table.insert ts t)
        (table.insert ts (- t))))
    ts))

(λ best-of [h1 h2 h3 time]
  (var bestdist 1e+300)
  (for [time time (+ time 10)]
    (let [ts (possible-time-offsets h1 h2 time)]
      (each [_ toffset (ipairs ts)]
        ; (for [toffset -10 10]
        (let [tvar (+ time toffset)]
          (when (> tvar 0)
            (let [[fixp fixd] h1
                  [varp vard] h2
                  fixv (V.add fixp (V.mul fixd time))
                  vecd* (V.sub (V.add varp (V.mul vard tvar))
                               fixv)
                  vecd (V.div vecd* toffset)
                  vecp (V.add fixv (V.mul vecd (- time)))
                  vec [vecp vecd]
                  dist3 (nearest-distance2 vec h3)]
              ; (print tfix tvar dist3)
              (if
                (= 0 dist3)
                (do
                  (printv "found:" vec)
                  (os.exit))
                (< dist3 bestdist)
                (set bestdist dist3))))))))
  bestdist)

(λ find-perfect-rock-position [hailstones]
  (let [h1 (. hailstones 1)
        h2 (. hailstones 2)
        h3 (. hailstones 3)]

    (var low 1)
    (var high 1e+14)
    (var done? false)
    (var bestlow (best-of h1 h2 h3 low))
    (var besthigh (best-of h1 h2 h3 high))

    (while (and (not done?)
                (> (- high low) 10))
      (let [test1 (math.floor (* (/ (- high low) 3) 1))
            test2 (math.ceil (* (/ (- high low) 3) 2))
            besttest1 (best-of h1 h2 h3 test1)
            besttest2 (best-of h1 h2 h3 test2)]
        (print low bestlow "-" high besthigh)
        (print test1 besttest1 "-" test2 besttest2)
        (print)
        (if
          (< test1 test2)
          (do
            (set high test2)
            (set besthigh besttest2))
          (do
            (set low test1)
            (set bestlow besttest1)))))))

(->> (read-input)
     ; print-interesting-numbers
     find-perfect-rock-position
     printv)
