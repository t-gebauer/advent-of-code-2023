;; reuseable functions

(λ map [fun list]
  (icollect [_ v (ipairs list)]
    (fun v)))

(λ apply [binary-op list]
  (accumulate [acc nil
               _ v (ipairs list)]
    (if (= nil acc)
      v
      (binary-op acc v))))

(λ make-pairs [list]
  (if (not= 0 (% (# list) 2) 0)
    (error "number of items must be divideable by 2"))
  (var last nil)
  (local result [])
  (each [_ item (ipairs list)]
    (if (= nil last)
      (set last item)
      (do
        (table.insert result [last item])
        (set last nil))))
  result)

;; puzzle specific logic

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
        (= seeds nil) (set seeds (->> line match-numbers make-pairs))
        (re.find line "'map:'") (do
                                  (set current-map {:name (line:sub 1 (- (# line) 1))})
                                  (table.insert maps current-map))
        :else ; must be a number line
        (table.insert current-map (match-numbers line))))
    {: seeds : maps}))

(λ apply-map-to-seed [map initial-seed]
  (local matched [])
  (var remaining initial-seed)
  (each [_ [dest source range] (ipairs map) &until (= remaining nil)]
    (let [seed remaining
          [s1 s2] seed
          m1 source
          m2 (+ source range -1)
          diff (- dest source)]
      ;; The mapping is sorted, so unless anything is outside the top range of the
      ;; map, we can stop immediately.
      (if
        ;; outside before
        (< s2 m1) (do
                    (table.insert matched seed)
                    (set remaining nil))
        ;; outside after
        (> s1 m2) nil
        ;; completely inside
        (and (>= s1 m1)
             (<= s2 m2)) (do
                           (table.insert matched [(+ s1 diff) (+ s2 diff)])
                           (set remaining nil))
        ;; lower out, upper in
        (and (< s1 m1)
             (>= s2 m1)
             (<= s2 m2)) (do
                           (table.insert matched [s1 (- m1 1)])
                           (table.insert matched [(+ m1 diff) (+ s2 diff)])
                           (set remaining nil))
        ;; lower in, upper out
        (and (>= s1 m1)
             (<= s1 m2)
             (> s2 m2)) (do
                          (table.insert matched [(+ s1 diff) (+ m2 diff)])
                          (set remaining [(+ m2 1) s2])))))
  ;; Interestingly, this extra check is not needed for my real input.
  (if (not= nil remaining)
    (table.insert matched remaining))
  matched)

(λ apply-all-maps-to-seeds [{: seeds : maps}]
  (accumulate [cur seeds
               _ map (ipairs maps)]
    (let [next []]
      (each [_ seed (ipairs cur)]
        (each [_ new-seed (ipairs (apply-map-to-seed map seed))]
          (table.insert next new-seed)))
      next)))

(λ sort-maps [{: maps &as state}]
  (each [_ map (ipairs maps)]
    (table.sort map (fn [[_ source-a _] [_ source-b _]] (< source-a source-b))))
  state)

(λ convert-seeds-to-ranges [{: seeds : maps}]
  (let [seeds (icollect [_ [start count] (ipairs seeds)]
                [start (+ start count -1)])]
    {: seeds : maps}))

(λ get-lower-bounds [[a _]] a)

(->> (parse-input)
     convert-seeds-to-ranges
     sort-maps
     apply-all-maps-to-seeds
     (map get-lower-bounds)
     (apply math.min)
     print)

