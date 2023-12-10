(local {: read-lines : map : printv : any : chars} (require :lib))

(λ find-start-position [lines]
  (each [y line (ipairs lines)]
    (each [x char (ipairs line)]
      (if (= char :S)
        (let [_pos [x y]]
          (lua "return _pos"))))))

(local north [0 -1])
(local south [0 1])
(local east [1 0 ])
(local west [-1 0])

(λ find-ways [lines [x y]]
  "Finds connected tiles, based on this tiles shape.
   Does not care if the neighboring shapes match."
  (let [shape (?. lines y x)
        rel (case shape
              :| [north south]
              :- [east west]
              :L [north east]
              :J [north west]
              :7 [south west]
              :F [south east]
              :. []
              :S []
              nil [])]
    (map (fn [[rx ry]]
           [(+ x rx)
            (+ y ry)])
         rel)))

(λ find-start-ways [lines start]
  (let [found-ways []
        [sx sy] start
        is-start? (fn [[x y]] (and (= x sx) (= y sy)))]
    (for [ox -1 1]
      (for [oy -1 1]
        (let [pos [(+ sx ox) (+ sy oy)]
              ways (find-ways lines pos)]
          (if (any is-start? ways)
            (table.insert found-ways pos)))))
    found-ways))

(λ find-start [lines]
  (let [start (find-start-position lines)
        ways (find-start-ways lines start)]
    {: lines
     : start
     : ways}))

(λ equal? [[ax ay] [bx by]]
  (and (= ax bx)
       (= ay by)))

(λ tset- [tbl [x y] v]
  "2d setter. Ignores out-of-bounds indices"
  (case (. tbl y)
    t (tset t x v)))

(λ tget- [tbl [x y]]
  "2d getter. Ignores out-of-bounds indices"
  (?. tbl y x))

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

(λ mark-the-loop [{: lines : start : ways}]
  (local loop (map #[] lines))
  (tset- loop start :S)
  (var prev start)
  (var [way _] ways)
  (while (not (equal? way start))
    (tset- loop way (tget- lines way))
    (let [next (case (find-ways lines way)
                 [a b] (if (equal? a prev) b a))]
      (set prev way)
      (set way next)))
  (local [h w] [(# lines)
                (# (. lines 1))])
  {: lines : start : ways : loop :size [w h]})

(λ rel-neighbours [shape dir side]
  (case [shape dir side]
    [:| :north :right] [east]
    [:| :north :left] [west]
    [:| :south :right] [west]
    [:| :south :left] [east]

    [:- :east :right] [south]
    [:- :east :left] [north]
    [:- :west :right] [north]
    [:- :west :left] [south]

    [:L :south :right] [west south]
    [:L :south :left] []
    [:L :west :right] []
    [:L :west :left] [west south]

    [:J :south :right] []
    [:J :south :left] [east south]
    [:J :east :right] [east south]
    [:J :east :left] []

    [:7 :north :right] [north east]
    [:7 :north :left] []
    [:7 :east :right] []
    [:7 :east :left] [north east]

    [:F :north :right] []
    [:F :north :left] [west north]
    [:F :west :right] [west north]
    [:F :west :left] []
    _ (error (.. "unmatched: " shape dir side))))

(λ mark-areas [{: size : loop : start : ways}]
  (local areas loop)
  (λ mark-on-side [side dir shape way]
    (each [_ [rx ry] (ipairs (rel-neighbours shape dir side))]
      (let [[wx wy] way
            pos [(+ wx rx) (+ wy ry)]]
        (if (= nil (tget- loop pos))
          (tset- areas pos (if (= side :right) :2 :1))))))
  (var prev start)
  (var [way _] ways)
  (var right-turns 0)
  (while (not (equal? way start))
    (let [dir (direction prev way)
          shape (tget- loop way)]
      (set right-turns (+ right-turns (right-turn-count dir shape)))
      (mark-on-side :right dir shape way)
      (mark-on-side :left dir shape way))
    (let [next (case (find-ways areas way)
                 [a b] (if (equal? a prev) b a))]
      (set prev way)
      (set way next)))
  {: size : areas
   :inner-marker (if (> right-turns 0) :2 :1)})

(λ dilate-areas [{:size [w h] : areas : inner-marker &as state}]
  (let [remaining []]
    (for [y 1 h]
      (for [x 1 w]
        (if (= inner-marker (tget- areas [x y]))
          (table.insert remaining [x y]))))
    (var current (table.remove remaining))
    (while current
      (local [cx cy] current)
      (for [y (- cy 1) (+ cy 1)]
        (for [x (- cx 1) (+ cx 1)]
          (case (tget- areas [x y])
            nil (do
                  (tset- areas [x y] inner-marker)
                  (table.insert remaining [x y])))))
      (set current (table.remove remaining))))
  state)

(λ count-area [{:size [w h] : areas : inner-marker}]
  (var area 0)
  (for [y 1 h]
    (for [x 1 w]
      (if (= inner-marker
             (tget- areas [x y]))
        (set area (+ area 1)))))
  area)

(->> (read-lines)
     (map chars)
     find-start
     mark-the-loop
     mark-areas
     dilate-areas
     count-area
     printv)
