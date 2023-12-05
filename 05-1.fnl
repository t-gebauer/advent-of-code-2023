(local re (require :re))

(λ match-numbers [line]
  (icollect [_ num (ipairs [(re.match line "[^0-9]* ({[0-9]+} / ' '+)+")])]
    (tonumber num)))

(λ parse-input []
  (var seeds nil)
  (var current-map nil)
  (let [maps {}]
    (each [line (io.lines)]
      (if (= line "") nil ; do nothing
        (= seeds nil) (set seeds (match-numbers line))
        (re.find line "'map:'") (do
                                  (set current-map {:name (line:sub 1 (- (# line) 1))})
                                  (table.insert maps current-map))
        :else ; must be a number line
        (table.insert current-map (match-numbers line))))
    {: seeds : maps}))

(λ apply-map-to-seed [map seed]
  (each [_ [dest source range] (ipairs map)]
    (if (and (>= seed source)
             (<= seed (+ source range)))
      (let [_next (+ seed (- dest source))]
        (lua "return _next" ))))
  seed)

(λ apply-all-maps-to-seeds [{: seeds : maps}]
  (icollect [_ seed (ipairs seeds)]
    (accumulate [cur seed
                 _ map (ipairs maps)]
      (apply-map-to-seed map cur))))

(λ find-closest-location [locations]
  (accumulate [closest nil
               _ location (ipairs locations)]
    (if (= nil closest)
      location
      (math.min closest location))))

(->> (parse-input)
     apply-all-maps-to-seeds
     find-closest-location
     print)

