"
Does not work on the example! Only on the real input.

This algorithm only works because of the following properties of the input map:
- The start is in the center
- No rocks at the border
- Straight, rock-free, lines from the center to all edges (Only in the real input, not the example!)

There are only two counts of maximally reachable plots for a map segment.
Those which can be reached with either an even or an odd number of steps.

There are only 4 types of squares (for each corner):
- completely filled
⁻ partially filled, straight from center
- filled corner type 1
- filled corner type 2

For the straight-reachable squares, the starting point is at the middle of the edge to the map center.
For the corner squares, the starting point is at the corner pointing to the map center.
"

(local {: read-lines : map : printv : chars} (require :lib))
(local grid2d (require :grid2d))
(local V (require :vec2))

;; Interesting (but irrelevant?):  26501365 = (202300 * grid.width) + ((grid.width - 1) / 2)
;; We cannot directly test with the example input, but we can
;; 1. test the naive implementation (21-2.fnl) with the example input
;; 2. check that this implementation gives the same answers as the naive one (for maps which are like the real input)
(local max-steps (or (tonumber (. arg 1)) 26501365))
(local plot-mod (% max-steps 2))

(λ count-reachable-plots [grid start max-steps*]
  "if max-steps = false -> search the whole map"
  (var count 0)
  (let [grid (grid2d.make (grid:as-table)) ; copying is only necessary when printing
        sgrid (grid2d.make-size* grid.size #false)
        remaining []]
    (var current start)
    (while current
      (let [[pos steps] current]
        (case (grid:get pos)
          :# nil ; ignore rocks
          nil nil ; ignore edges
          _
          (let [s (sgrid:get pos)]
            (when (and (= false s)
                       (= (% steps 2) plot-mod))
              (set count (+ count 1))
              (grid:set pos :O))
            (when (or (= false s)
                      (< steps s))
              (sgrid:set pos steps)
              (when (or (= false max-steps*)
                        (< steps max-steps*))
                (table.insert remaining [(V.add pos V.north) (+ steps 1)])
                (table.insert remaining [(V.add pos V.south) (+ steps 1)])
                (table.insert remaining [(V.add pos V.west) (+ steps 1)])
                (table.insert remaining [(V.add pos V.east) (+ steps 1)]))))))
      (set current (table.remove remaining 1)))
    ; (grid:print)
    ; (print)
    count))

(λ count-reachable-plots* [grid start-pos max-steps* mod]
  (count-reachable-plots grid [start-pos mod] (+ max-steps* mod)))

(λ triangular-number [n]
  (/ (* n (+ n 1)) 2))

(λ count-all-reachable-plots [grid]
  (let [start (grid:find-one :S)
        [sx sy] start
        steps-over-edge (/ (+ grid.width 1) 2)
        steps-over-corner (+ grid.width 1)
        plots-in-full-square (count-reachable-plots grid [start 0] false)
        plots-in-full-square* (count-reachable-plots grid [start 1] false)
        number-of-straight-squares (math.floor (/ (- max-steps steps-over-edge) grid.width))
        full-corner-layers (- (math.floor (/ (- max-steps steps-over-corner) grid.width)) 1)
        ;; Determine how many steps are left on the outer-most squares
        remaining-straight (% (- max-steps steps-over-edge) grid.width)
        remaining-corner (% (- max-steps steps-over-corner) grid.width)
        remaining-corner* (+ remaining-corner grid.width)
        ;; Half of the squares are "inverted", mod 2 = 0 instead of 1
        number-of-straight-squares* (math.ceil (/ number-of-straight-squares 2))
        number-of-straight-squares** (math.floor (/ number-of-straight-squares 2))
        ;; We can count the number of corner squares by counting outwards: 1+2+3+4+...+n => n*(n+1)/2
        ;; But, because of mod 2, we need to count each second layer separately: 1+3+5+...+m and 2+4+6+...+m where n is m <= n
        max-corner-layer* (- (* (math.ceil (/ full-corner-layers 2)) 2) 1) ; 8=>7, 7=>7, 6=>5
        max-corner-layer** (* (math.floor (/ full-corner-layers 2)) 2) ; 8=>8, 7=>6, 6=>6
        full-corner-squares* (+ (triangular-number (math.ceil (/ max-corner-layer* 2)))
                                (triangular-number (math.floor (/ max-corner-layer* 2))))
        full-corner-squares** (* 2 (triangular-number (/ max-corner-layer** 2)))
        straight-mod (% number-of-straight-squares 2)
        corner-mod (% (+ full-corner-layers 1) 2)
        corner-mod* (% full-corner-layers 2)]
    (assert (> max-steps steps-over-edge) "not enough steps to reach adjacent square")
    (assert (> max-steps steps-over-corner) "not enough steps to reach diagonal square")
    (printv "start" start)
    (printv "size" grid.size)
    (printv "steps over edge" steps-over-edge)
    (printv "steps over corner" steps-over-corner)
    (print "full corner layers" full-corner-layers)
    (print "remaining straight" remaining-straight)
    (print "remaining corner" remaining-corner remaining-corner*)
    (print "plots in full" plots-in-full-square)
    (print "plots in full alt" plots-in-full-square*)
    (+
     ;; Full squares; "normal" (same remainder as the start square)
     (* plots-in-full-square
        (+ 1 ; the start
           (* 4 number-of-straight-squares**)
           (* 4 full-corner-squares*)))
     ;; Full squares; "inverted" (different remainder as the start square)
     (* plots-in-full-square*
        (+ (* 4 number-of-straight-squares*)
           (* 4 full-corner-squares**)))
     ;; Partially filled
     (count-reachable-plots* grid [sx grid.height] remaining-straight straight-mod) ; straight north
     (count-reachable-plots* grid [grid.width sy]  remaining-straight straight-mod) ; straight west
     (count-reachable-plots* grid [sx 1]           remaining-straight straight-mod) ; straight south
     (count-reachable-plots* grid [1 sy]           remaining-straight straight-mod) ; straight east
     (* (+ 2 full-corner-layers)
        (+ (count-reachable-plots* grid [grid.width grid.height] remaining-corner corner-mod) ; corner nw
           (count-reachable-plots* grid [grid.width 1]           remaining-corner corner-mod) ; corner sw
           (count-reachable-plots* grid [1 grid.height]          remaining-corner corner-mod) ; corner ne
           (count-reachable-plots* grid [1 1]                    remaining-corner corner-mod))) ; corner se
     (* (+ 1 full-corner-layers)
        (+ (count-reachable-plots* grid [grid.width grid.height] remaining-corner* corner-mod*) ; corner nw
           (count-reachable-plots* grid [grid.width 1]           remaining-corner* corner-mod*) ; corner sw
           (count-reachable-plots* grid [1 grid.height]          remaining-corner* corner-mod*) ; corner ne
           (count-reachable-plots* grid [1 1]                    remaining-corner* corner-mod*))))))

(->> (read-lines)
     (map chars)
     grid2d.make
     count-all-reachable-plots
     (string.format "%0.f")
     print)

