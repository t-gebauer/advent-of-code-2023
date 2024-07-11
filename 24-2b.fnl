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

(λ find-perfect-rock-position [hailstones]
  ;; Choose a starting hailstone
  (let [[onep oned] (. hailstones 1)]
    ;; Check all timestamps
    (for [t1 1 1e+14]
      (if (= 0 (% t1 1e+2))
        (print "checking" t1))
      (let [refv (V.add onep (V.mul oned t1))
            [secp secd] (. hailstones 2)
            [dx dy dz] (V.sub (V.add secp (V.mul secd t1))
                              refv)]
        ;; Timestamps are only valid if the difference to other hailstones is an integer divideable by the time difference
        ;; We only need one list of dividers, in a non-zero dimension.
        (let [divs (or (if (not= 0 dx) (dividers dx))
                       (if (not= 0 dy) (dividers dy))
                       (if (not= 0 dz) (dividers dz)))
              ts []]
          (each [_ t (ipairs divs)]
            (when (and (= 0 (% dx t))
                       (= 0 (% dy t))
                       (= 0 (% dz t)))
              (table.insert ts t)
              (table.insert ts (- t))))

          (each [_ t (ipairs ts)]
            (let [t2 (+ t1 t)
                  vecd (V.sub (V.add secp (V.mul secd t2))
                              refv)
                  vecd (V.div vecd t)
                  vecp (V.add refv (V.mul vecd (- t1)))
                  vec [vecp vecd]
                  dist3 (nearest-distance2 vec (. hailstones 3))]
              (when (= dist3 0)
                (printv "done?" vec)
                (os.exit)))))))))


(->> (read-input)
     find-perfect-rock-position
     printv)
