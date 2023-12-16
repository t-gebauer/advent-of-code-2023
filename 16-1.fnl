(local {: read-lines : map : printv : chars} (require :lib))
(local grid2d (require :grid2d))
(local V (require :vec2))

(λ reflect [type dir]
  (case [type dir]
    [:/ :up] :right
    [:/ :down] :left
    [:/ :left] :down
    [:/ :right] :up
    [:\ :up] :left
    [:\ :down] :right
    [:\ :left] :up
    [:\ :right] :down
    _ (error "unmatched")))

(λ follow-beams [grid]
  (let [beams [[[0 1] :right]]
        meta (grid2d.make-size* grid.size #{:energized false
                                            :has-beam-in-dir? {}})]
    (while (> (# beams) 0)
      (let [[prev dir] (table.remove beams)
            pos (V.add prev (. V dir))
            m (meta:get pos)]
        (when m ; m is nil when pos is outside the grid
          (set m.energized true)
          (when (not (. m.has-beam-in-dir? dir))
            (tset m.has-beam-in-dir? dir true)
            (->> (case (grid:get pos)
                   :. [[pos dir]]
                   :/ [[pos (reflect :/ dir)]]
                   :\ [[pos (reflect :\ dir)]]
                   :| (if (or (= dir :up)
                              (= dir :down))
                        [[pos dir]]
                        [[pos :up]
                         [pos :down]])
                   :- (if (or (= dir :left)
                              (= dir :right))
                        [[pos dir]]
                        [[pos :left]
                         [pos :right]])
                   nil [] ; out of grid
                   _ (error "unmatched"))
                 (map #(table.insert beams $1)))))))
    (length (meta:find-all* #(do $1.energized)))))

(->> (read-lines)
     (map chars)
     grid2d.make
     follow-beams
     printv)
