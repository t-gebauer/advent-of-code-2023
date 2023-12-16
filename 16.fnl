(local {: read-lines : map : printv : chars} (require :lib))
(local grid2d (require :grid2d))
(local V (require :vec2))

(local is-part-1? (= :1 (or (. arg 1) :1)))

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

(λ count-energized-tiles [grid initial-beam]
  (let [beams [initial-beam]
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

(λ follow-all-beams [grid]
  (assert (= grid.width grid.height))
  (var max 0)
  (each [_ dir (ipairs [:up :down :left :right])]
    (for [i 1 grid.width]
      (let [start-pos (case dir
                        :right [0 i]
                        :left [(+ grid.width 1) i]
                        :down [i 0]
                        :up [i (+ grid.height 1)])
            energized-count (count-energized-tiles grid [start-pos dir])]
        (if (> energized-count max)
          (set max energized-count)))))
  max)

(λ follow-beams [grid]
  (if is-part-1?
    (count-energized-tiles grid [[0 1] :right])
    (follow-all-beams grid)))

(->> (read-lines)
     (map chars)
     grid2d.make
     follow-beams
     printv)
