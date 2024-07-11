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

(λ shuffle [hailstones]
  (let [l (# hailstones)]
    (for [i 1 l]
      (let [this (. hailstones i)
            o (math.random l)
            other (. hailstones o)]
        (tset hailstones i other)
        (tset hailstones o this)))))

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


;; Speed up the search a lot, by some kind of bad binary search ...
;; This does not even work reliably for the example input.
(λ find-perfect-rock-position [hailstones]
  ;; Success depends on luck :)
  (shuffle hailstones)
  (shuffle hailstones)
  (shuffle hailstones)

  (var done? false)
  (var found nil)
  (let [[startp startd] (. hailstones 1)
        [secp secd] (. hailstones 2)]

    (λ dist [startt]
      (var sum 0)
      (var count 0)
      (let [tcheck 1e+5
            [start end] [(math.max 1 (- startt tcheck)) (+ startt tcheck)]
            startv (V.add startp (V.mul startd startt))]
        (for [sect start end]
          (when (not= startt sect)
            (let [secv (V.add secp (V.mul secd sect))
                  vec (V.sub secv startv)
                  tdiff (- sect startt)
                  sec-reachable? (and
                                   (= 0 (% (. vec 1) tdiff))
                                   (= 0 (% (. vec 2) tdiff))
                                   (= 0 (% (. vec 3) tdiff)))
                  vecd (V.div vec tdiff)
                  vecp (V.sub startv (V.mul vecd startt))
                  vec [vecp vecd]]
              (when sec-reachable?
                (let [dist3 (nearest-distance2 vec (. hailstones 3))]
                  (set count (+ count 1))
                  (set sum (+ sum dist3))
                  (when (= 0 dist3)
                    (var abort-at nil)
                    (each [i other (ipairs hailstones) &until abort-at]
                      (when (> i 3)
                        (let [dist (nearest-distance2 vec other)]
                          (when (not= 0 dist)
                            (printv vec "failed at" i)
                            (set abort-at i)))))
                    (when (= nil abort-at)
                      (printv "done" vec "at" startt sect)
                      (set done? true)
                      (set found vec))))))))
        (/ sum count)))

    (var near 1)
    (var far 1e+14)
    ; (var near 300000000000)
    ; (var far 400000000000)
    (var dnear (dist near))
    (var dfar (dist far))
    (while (and (> (- far near) 2); 1e+4)
                (not done?))
      ;; search at 1/3 and 2/3
      (let [startt (+ near (math.floor (/ (- far near) 3)))
            startt* (+ near (math.ceil (/ (* (- far near) 2) 3)))]
        (let [res (dist startt)
              res* (dist startt*)]
          (print "near" near "far" far)
          (print near dnear "-" far dfar)
          (print startt res "-" startt* res*)
          (if
            (or
              (and (> res dnear)
                   (> res dfar))
              (and (> res* dnear)
                   (> res* dfar)))
            (do
              (set near (+ near 1))
              (set dnear (dist near))
              (set far (- far 1))
              (set dfar (dist far)))

            (and (< dnear res)
                 (< dnear res*))
            (do
              (set far startt*)
              (set dfar res*))

            (and (< dfar res)
                 (< dfar res*))
            (do
              (set near startt)
              (set dnear res))

            (< res res*)
            (do
              (set far startt*)
              (set dfar res*))

            (do
              (set near startt)
              (set dnear res)))
          nil)))
    (print "finishing" near "to" far)
    (for [t near far &until done?]
      (dist t)))
  (if found
    (printv found (+ (. found 1 1)
                     (. found 1 2)
                     (. found 1 3)))
    (do
      (print "NOT FOUND")
      (os.exit false))))

(->> (read-input)
     find-perfect-rock-position
     printv)
