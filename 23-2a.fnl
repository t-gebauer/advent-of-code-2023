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
        remaining [[start start 0 (V.add start V.north)]]
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
    {: grid : nodes}))

(λ copy-table [tbl]
  (collect [k v (pairs tbl)]
    k v))

;; This is not ideal. Takes too long, calculates same paths multiple times,
;; but it works (in less than a minute).
(λ find-longest-path [{: grid : nodes}]
  (local start (index [2 1]))
  (local goal (index (V.add grid.size [-1 0])))
  (λ find-longest-path* [current used-nodes]
    (tset used-nodes current true)
    (if (= current goal)
      [used-nodes 0]
      (do
        (var longest 0)
        (var longest-nodes nil)
        (each [node len (pairs (. nodes current))]
          (if (not (. used-nodes node))
            (let [used-nodes (copy-table used-nodes)
                  [returned-nodes next-len] (find-longest-path* node used-nodes)
                  path-len (+ len next-len)]
              (when (and (> path-len longest)
                         (not= returned-nodes nil))
                (set longest path-len)
                (set longest-nodes returned-nodes)))))
        [longest-nodes longest])))
  (find-longest-path* start {}))

(->> (read-lines)
     (map chars)
     grid2d.make
     build-graph
     find-longest-path
     printv)
