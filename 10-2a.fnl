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

(λ direction [[ax ay] [bx by]]
  (case [(- bx ax)
         (- by ay)]
    [-1 0] :west
    [1 0] :east
    [0 -1] :north
    [0 1] :south))

(λ right-turn-count [dir shape]
  (case [dir shape]
    [:south :L] -1
    [:west :L] 1
    [:south :J] 1
    [:east :J] -1
    [:east :7] 1
    [:north :7] -1
    [:west :F] -1
    [:north :F] 1
    _ 0))

(λ find-the-loop [{: grid : start : ways}]
  (local loop (grid2d.make-size grid.size :.))
  (loop:set start :S)
  (var prev start)
  (var [way1 way2] ways)
  (var way way1)
  (var right-turns 0)
  (while (not (V.equal? way start))
    (loop:set way (grid:get way))
    (let [dir (direction prev way)
          shape (grid:get way)]
      (set right-turns (+ right-turns (right-turn-count dir shape))))
    (let [next (case (find-neighbors grid way)
                 [a b] (if (V.equal? a prev) b a))]
      (set prev way)
      (set way next)))
  {: grid : start : ways
   : loop :right-way (if (> right-turns 0) way1 way2)})

(λ rel-neighbours [shape dir]
  (case [shape dir]
    [:| :north] [east]
    [:| :south] [west]
    [:- :east] [south]
    [:- :west] [north]
    [:L :south] [west south]
    [:L :west] []
    [:J :south] []
    [:J :east] [east south]
    [:7 :north] [north east]
    [:7 :east] []
    [:F :north] []
    [:F :west] [west north]
    _ (error (.. "unmatched: " shape dir))))

(λ mark-areas [{: loop : start : right-way}]
  (local areas loop)
  (λ mark-on-side [dir shape way]
    (each [_ [rx ry] (ipairs (rel-neighbours shape dir))]
      (let [[wx wy] way
            pos [(+ wx rx) (+ wy ry)]]
        (if (= :. (loop:get pos))
          (areas:set pos :I)))))
  (var prev start)
  (var way right-way)
  (while (not (V.equal? way start))
    (let [dir (direction prev way)
          shape (loop:get way)]
      (mark-on-side dir shape way))
    (let [next (case (find-neighbors areas way)
                 [a b] (if (V.equal? a prev) b a))]
      (set prev way)
      (set way next)))
  {: areas})

(λ dilate-areas [{: areas &as state}]
  (let [remaining (areas:find-all :I)]
    (var current (table.remove remaining))
    (while current
      (local [cx cy] current)
      (for [y (- cy 1) (+ cy 1)]
        (for [x (- cx 1) (+ cx 1)]
          (case (areas:get [x y])
            :. (do
                 (areas:set [x y] :I)
                 (table.insert remaining [x y])))))
      (set current (table.remove remaining))))
  state)

(λ count-area [{: areas}]
  (length (areas:find-all :I)))

(->> (read-lines)
     (map chars)
     grid2d
     find-start
     find-the-loop
     mark-areas
     dilate-areas
     count-area
     printv)
