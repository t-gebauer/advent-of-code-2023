(local {: read-lines : map : printv : chars} (require :lib))
(local grid2d (require :grid2d))
(local V (require :vec2))

(λ count-steps [grid]
  (local max (if (< grid.width 12) 6 64))
  (var count 0)
  (let [sgrid (grid2d.make-size* grid.size #nil)
        start (grid:find-one :S)
        remaining []]
    (var current [start 0])
    (while current
      (let [[pos steps] current]
        (case (grid:get pos)
          :# nil ; ignore rocks
          nil nil ; ignore
          _
          (let [s (sgrid:get pos)]
            (when (and (= nil s)
                       (= 0 (% steps 2)))
              (set count (+ count 1))
              (grid:set pos :0))
            (when (or (= nil s)
                      (< steps s))
              (sgrid:set pos steps)
              (when (< steps max)
                (table.insert remaining [(V.add pos V.north) (+ steps 1)])
                (table.insert remaining [(V.add pos V.south) (+ steps 1)])
                (table.insert remaining [(V.add pos V.west) (+ steps 1)])
                (table.insert remaining [(V.add pos V.east) (+ steps 1)]))))))
      (set current (table.remove remaining))))
  (if (< grid.width 12)
    (grid:print))
  count)

(->> (read-lines)
     (map chars)
     grid2d.make
     count-steps
     printv)
