(local {: read-lines : map : printv : any : chars} (require :lib))

(λ find-start-position [lines]
  (each [y line (ipairs lines)]
    (each [x char (ipairs line)]
      (if (= char :S)
        (let [_pos [x y]]
          (lua "return _pos"))))))

(λ find-ways [lines [x y]]
  "Finds connected tiles, based on this tiles shape.
   Does not care if the neighboring shapes match."
  (let [shape (. lines y x)
        north [0 -1]
        south [0 1]
        east [1 0]
        west [-1 0]
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

(λ find-longest-distance [{: lines : start : ways}]
  (var longest 1)
  (var [prev1 prev2] [start start])
  (var [way1 way2] ways)
  (while (not (equal? way1 way2))
    (set longest (+ longest 1))
    (let [next1 (case (find-ways lines way1)
                  [a b] (if (equal? a prev1) b a))
          next2 (case (find-ways lines way2)
                  [a b] (if (equal? a prev2) b a))]
      (set prev1 way1)
      (set prev2 way2)
      (set way1 next1)
      (set way2 next2)))
  longest)

(->> (read-lines)
     (map chars)
     find-start
     find-longest-distance
     printv)
