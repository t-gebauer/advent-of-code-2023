(local {: read-lines : map : printv : chars} (require :lib))
(local grid2d (require :grid2d))
(local V (require :vec2))

;; All branches have slopes, so we only need to remember the last position for
;; each path that we take, and not all previous positions, or all branching
;; points.

(λ find-longest-hike [grid]
  (var longest-hike 0)
  (let [start [2 1]
        goal (V.add grid.size [-1 0])
        remaining [[start 0 (V.add start V.north)]]]
    (while (> (# remaining) 0)
      (local [pos len prev] (table.remove remaining))
      (if (V.equal? pos goal)
        (set longest-hike (math.max longest-hike len)))
      (let [
            dirs [[:^ :north]
                  [:v :south]
                  [:< :west]
                  [:> :east]]
            paths (icollect [_ [c d] (ipairs dirs)]
                    (let [p (V.add pos (. V d))
                          char (grid:get p)]
                      (if (and (not (V.equal? p prev))
                               (or (= char c)
                                   (= char :.)))
                        [p (+ 1 len) pos])))]
        (each [_ path (ipairs paths)]
          (table.insert remaining path)))))
  longest-hike)

(->> (read-lines)
     (map chars)
     grid2d.make
     find-longest-hike
     printv)
