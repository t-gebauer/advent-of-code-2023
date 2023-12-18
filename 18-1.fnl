(local {: read-lines : map : printv} (require :lib))
(local grid2d (require :grid2d))
(local V (require :vec2))
(local re (require :re))

(local pattern (re.compile "{| {[RLUD]} ' ' {%d+} ' (#' {[a-f%d]+} ')' |}"))

(λ direction [char]
  (case char
    :R V.right
    :L V.left
    :U V.up
    :D V.down))

(λ dig-trench [plan]
  (let [grid (grid2d.make-size [1 1] :#)]
    (var pos [1 1])
    (each [_ [d len _color] (ipairs plan)]
      (let [dir (direction d)
            char (case d
                   :U :^
                   :D :v
                   _ :-)]
        (if (or (= d :U)
                (= d :D))
          (grid:force-set pos char))
        (for [i 1 len]
          (set pos (V.add pos dir i))
          (grid:force-set pos char))))
    (grid:print-size)
    grid))

(λ count-interior [grid]
  (var count 0)
  (for [y (or grid.negy 1) grid.height]
    (var inside? false)
    (var dir nil)
    (for [x (or grid.negx 1) grid.width]
      (case (grid:get [x y])
        :^ (do
             (set count (+ count 1))
             (case dir
               :up (do
                     (set inside? (not inside?))
                     (set dir nil))
               :down (set dir nil)
               nil (set dir :up)))
        :v (do
             (set count (+ count 1))
             (case dir
               :up (set dir nil)
               :down (do
                       (set inside? (not inside?))
                       (set dir nil))
               nil (set dir :down)))
        :- (set count (+ count 1))
        nil (do
              (case dir
                :up (set inside? (not inside?))
                :down (set inside? (not inside?)))
              (set dir nil)
              (if inside?
                (set count (+ count 1)))))))
  count)

(->> (read-lines)
     (map #(pattern:match $1))
     dig-trench
     count-interior
     printv)
