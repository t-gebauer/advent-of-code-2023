(local {: read-lines : map : printv : chars} (require :lib))
(local grid2d (require :grid2d))
(local V (require :vec2))

(λ path-index [path]
  (let [[px py] path.pos
        [dx dy] path.dir]
    (.. px :: py :: dx :: dy :: path.dir-count)))

(λ get-or-create [outer index]
  (case (. outer index)
    inner inner
    nil (let [new []]
          (tset outer index new)
          new)))

(λ simulate [blocks]
  (let [goal [blocks.width blocks.height]
        max-heat-loss 999 ; a large number
        remaining-paths {0 [{:pos [1 1]
                             :dir V.right
                             :dir-count 0
                             :heat-loss 0}
                            {:pos [1 1]
                             :dir V.down
                             :dir-count 0
                             :heat-loss 0}]}
        known-paths {}]
    (var lowest-heat-loss max-heat-loss)
    (var current-path-rating 0)
    (while (< current-path-rating lowest-heat-loss)
      (var current nil)
      (for [i current-path-rating max-heat-loss &until current]
        (case (. remaining-paths i)
          paths (case (table.remove paths)
                  path (do
                         (set current path)
                         (set current-path-rating i)))))
      (local possible-steps [])
      (when (>= current.dir-count 4)
        (table.insert possible-steps [(V.turn-left current.dir) 1])
        (table.insert possible-steps [(V.turn-right current.dir) 1]))
      (if (< current.dir-count 10)
        (table.insert possible-steps [current.dir (+ current.dir-count 1)]))
      (each [_ [dir dir-count] (ipairs possible-steps)]
        (local pos (V.add current.pos dir))
        (case (blocks:get pos) ; implicitly ignores paths outside of the grid
          block-heat-loss
          (let [heat-loss (+ current.heat-loss block-heat-loss)]
            (if (and (V.equal? pos goal)
                     (>= dir-count 4))
              (set lowest-heat-loss (math.min lowest-heat-loss heat-loss))
              (let [path-rating (+ heat-loss (V.abs (V.sub goal pos)))
                    path {: pos : dir : dir-count : heat-loss}
                    index (path-index path)]
                (when (not (. known-paths index))
                  (tset known-paths index true)
                  (table.insert (get-or-create remaining-paths path-rating) path))))))))
    lowest-heat-loss))

(->> (read-lines)
     (map chars)
     (map #(map tonumber $1))
     grid2d.make
     simulate
     printv)
