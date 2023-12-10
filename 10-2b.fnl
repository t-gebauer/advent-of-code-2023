(local {: read-lines : map : printv : any : chars} (require :lib))
(local grid2d (require :grid2d))
(local {: north : south : east : west &as V} (require :vec2))

(λ neighbors [?shape]
  (case ?shape
    :| [north south]
    :- [east west]
    :L [north east]
    :J [north west]
    :7 [south west]
    :F [south east]
    :. []
    :S []
    nil []))

(λ find-neighbors [grid [x y]]
  "Finds connected tiles, based on this tiles shape.
   Does not care if the neighboring shapes match."
  (let [shape (grid:get [x y])
        rel (neighbors shape)]
    (map (fn [[rx ry]]
           [(+ x rx)
            (+ y ry)])
         rel)))

(λ find-start-ways [grid start]
  (let [found-ways []
        is-start? #(V.equal? $1 start)]
    (for [ox -1 1]
      (for [oy -1 1]
        (let [pos (V.add start [ox oy])
              ways (find-neighbors grid pos)]
          (if (any is-start? ways)
            (table.insert found-ways pos)))))
    found-ways))

(λ find-start [grid]
  (let [start (grid:find-one :S)
        ways (find-start-ways grid start)]
    {: grid
     : start
     : ways}))

(λ find-start-shape [start [way1 way2]]
  (each [_ shape (ipairs [:| :- :L :J :7 :F])]
    (let [[a b] (->> (neighbors shape)
                    (map #(V.add $1 start)))]
      (if (or (and (V.equal? a way1) (V.equal? b way2))
              (and (V.equal? a way2) (V.equal? b way1)))
        (lua "return shape")))))

(λ find-the-loop [{: grid : start : ways}]
  (local loop (grid2d.make-size grid.size :.))
  (loop:set start (find-start-shape start ways))
  (var prev start)
  (var [way1 _] ways)
  (var way way1)
  (while (not (V.equal? way start))
    (loop:set way (grid:get way))
    (let [next (case (find-neighbors grid way)
                 [a b] (if (V.equal? a prev) b a))]
      (set prev way)
      (set way next)))
  {: loop})

(λ count-inside [{: loop}]
  (var count 0)
  (for [y 1 loop.height]
    (var inside? false)
    (var from-north? false)
    (for [x 1 loop.width]
      (let [pos [x y]
            shape (loop:get pos)]
        (case shape
          :. (if inside? (set count (+ count 1)))
          :| (set inside? (not inside?))
          :- nil
          :L (set from-north? true)
          :J (if (not from-north?) (set inside? (not inside?)))
          :7 (if from-north? (set inside? (not inside?)))
          :F (set from-north? false)))))
  count)

(->> (read-lines)
     (map chars)
     grid2d
     find-start
     find-the-loop
     count-inside
     printv)
