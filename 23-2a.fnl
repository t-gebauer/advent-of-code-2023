(local {: read-lines : map : printv : chars} (require :lib))
(local grid2d (require :grid2d))
(local V (require :vec2))

(λ index [[x y]]
  (.. x "," y))

(λ insert-max [tbl a b len]
  (let [A (case (. tbl (index a))
            t t
            nil (let [new {}]
                  (tset tbl (index a) new)
                  new))
        len (math.max (or (. A (index b)) 0)
                      len)]
    (tset A (index b) len)))

(λ build-graph [grid]
  (let [start [2 1]
        goal (V.add grid.size [-1 0])
        nodes {(index start) {}
               (index goal) {}}
        remaining [[start start 1 (V.add start V.north)]]
        dirs [:north :south :west :east]]
    (while (> (# remaining) 0)
      (local [node pos len prev] (table.remove remaining))
      (let [paths (icollect [_ d (ipairs dirs)]
                    (let [p (V.add pos (. V d))
                          char (grid:get p)]
                      (if (and (not (V.equal? p prev))
                               (not= char :#)
                               (not= char nil))
                        p)))]
        (if (not= (# paths) 1)
          (do
            (local already-visited? (. nodes (index pos)))
            (insert-max nodes node pos len)
            (insert-max nodes pos node len)
            (if (not already-visited?)
              (each [_ path (ipairs paths)]
                (table.insert remaining [pos path 1 pos]))))
          (do
            (table.insert remaining [node (. paths 1) (+ len 1) pos])))))
    nodes))

(λ find-longest-path [nodes]
  (local longest {})
  ;; TODO
  )

(->> (read-lines)
     (map chars)
     grid2d.make
     build-graph
     find-longest-path
     printv)
