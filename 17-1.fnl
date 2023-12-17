(local {: read-lines : map : printv : chars} (require :lib))
(local grid2d (require :grid2d))
(local V (require :vec2))

(λ path-index [path]
  (let [[px py] path.pos
        [dx dy] path.dir]
    (.. px :: py :: dx :: dy :: path.dir-count)))

(λ simulate [blocks]
  (let [goal [blocks.width blocks.height]
        max-heat-loss (* 9 (+ blocks.width blocks.height))
        remaining-paths {0 [{:pos [1 1]
                             :dir V.up ; irrelevant with count 0
                             :dir-count 0
                             :heat-loss 0}]}
        known-paths {}]
    (var lowest-heat-loss max-heat-loss)
    (var current-path-rating 0)
    (while (<= current-path-rating lowest-heat-loss)
      (var current nil)
      (for [i current-path-rating max-heat-loss &until current]
        (case (. remaining-paths i)
          paths (case (table.remove paths)
                  path (do
                         (set current path)
                         (set current-path-rating i)))))
      (local possibe-steps [[(V.turn-left current.dir) 1]
                            [(V.turn-right current.dir) 1]
                            (if (< current.dir-count 3)
                              [current.dir (+ current.dir-count 1)])])
      (each [_ [dir dir-count] (ipairs possibe-steps)]
        (local pos (V.add current.pos dir))
        (case (blocks:get pos) ; implicitly ignores paths outside of the grid
          block-heat-loss
          (let [heat-loss (+ current.heat-loss block-heat-loss)]
            (if (V.equal? pos goal)
              (set lowest-heat-loss (math.min lowest-heat-loss heat-loss))
              (let [path-rating (+ heat-loss (V.abs (V.sub goal pos)))
                    paths-for-rating (case (. remaining-paths path-rating)
                                       paths paths
                                       nil (let [paths []]
                                             (tset remaining-paths path-rating paths)
                                             paths))]
                (if (< path-rating lowest-heat-loss)
                  (let [path {: pos
                              : dir
                              : dir-count
                              : heat-loss }
                        index (path-index path)]
                    (when (not (. known-paths index))
                      (tset known-paths index true)
                      (table.insert paths-for-rating path))))))))))
    lowest-heat-loss))

(->> (read-lines)
     (map chars)
     (map #(map tonumber $1))
     grid2d.make
     simulate
     printv)
