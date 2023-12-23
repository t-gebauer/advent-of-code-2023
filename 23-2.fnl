;;; Too slow

(local {: read-lines : map : printv : chars} (require :lib))
(local grid2d (require :grid2d))
(local V (require :vec2))

(λ index [[x y]]
  (.. x "," y))

(λ find-longest-hike [grid]
  (var longest-hike 0)
  (let [start [2 1]
        goal (V.add grid.size [-1 0])
        remaining [[start 0 {}]]]
    (while (> (# remaining) 0)
      (local [pos len visited] (table.remove remaining))
      (if (V.equal? pos goal)
        (set longest-hike (math.max longest-hike len)))
      (let [dirs [:north :south :west :east]
            paths (icollect [_ d (ipairs dirs)]
                    (let [p (V.add pos (. V d))
                          char (grid:get p)]
                      (if (and (not (. visited (index p)))
                               (not= char :#)
                               (not= char nil))
                        [p (+ 1 len)])))]
        (if (> (# paths) 1)
          (tset visited (index pos) true))
        (var needs-to-clone? false)
        (each [_ [pos len] (ipairs paths)]
          (let [visited (if needs-to-clone?
                          (collect [k v (pairs visited)]
                            k v)
                          visited)]
            (table.insert remaining [pos len visited]))
          (set needs-to-clone? true)))))
  longest-hike)

(->> (read-lines)
     (map chars)
     grid2d.make
     find-longest-hike
     printv)
