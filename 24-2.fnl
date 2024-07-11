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

(λ nearest-distance [linea lineb]
  (let [[ap ad] linea
        [bp bd] lineb]
    (var closest nil)
    (var done? false)
    (for [t 1 1e+12 &until done?] ; careful, this might take a while
      (let [av (V.add ap (V.mul ad t))
            bv (V.add bp (V.mul bd t))
            d (V.length (V.sub bv av))]
        (if (and
              (not= nil closest)
              (> d closest))
          (set done? true)
          (set closest d))))
    closest))

;; Brute force the example, by iterating through all possible timing
;; combinations of the first and second hailstone.
;; Check if the combination is valid, by dividing the distance by the time difference.
;; If it is neatly divideable, check the distance between the resulting rock through and
;; all other hailstones.
(λ find-perfect-rock-position [hailstones]
  (var found nil)
  (let [tmax 1e+6
        [startp startd] (. hailstones 1)
        [secp secd] (. hailstones 2)]
    (for [startt 1 tmax &until (not= nil found)]
      (if (= 0 (% startt 1e+2))
        (print "checking" startt))
      (let [startv (V.add startp (V.mul startd startt))]
        (var done? false)
        (for [sect 1 tmax &until done?]
          (when (not= startt sect)
            (let [secv (V.add secp (V.mul secd sect))
                  v (V.sub secv startv)
                  tdiff (- sect startt)
                  reachable? (and
                               (= 0 (% (. v 1) tdiff))
                               (= 0 (% (. v 2) tdiff))
                               (= 0 (% (. v 3) tdiff)))
                  vecd (V.div v tdiff)
                  vecp (V.sub startv (V.mul vecd startt))
                  vec [vecp vecd]]
              (when reachable?
                (var abort-at nil)
                (each [i other (ipairs hailstones) &until abort-at]
                  (when (> i 2)
                    (let [dist (nearest-distance vec other)]
                      (when (not= 0 dist)
                        (if (> i 3)
                          (printv vec "failed at" i)) ;; never happening
                        (set abort-at i)))))
                (when (= nil abort-at)
                  (set done? true)
                  (set found vec)))))))))
  found)

(->> (read-input)
     find-perfect-rock-position
     printv)
