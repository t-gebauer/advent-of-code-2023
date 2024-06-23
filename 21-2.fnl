;;; Does not solve part 2

(local {: read-lines : map : printv : chars} (require :lib))
(local grid2d (require :grid2d))
(local V (require :vec2))

(local max (tonumber (. arg 1)))

(λ count-steps [grid]
  (printv "size" grid.size)
  (var count 0)
  (var max-steps-used 0)
  (var min-north nil)
  (var min-south nil)
  (var min-west nil)
  (var min-east nil)
  (let [sgrid (grid2d.make-size* grid.size #false)
        start (grid:find-one :S)
        remaining []]
    (var current [start 0])
    (while current
      (let [[pos steps] current]
        ; (printv max-steps-used steps)
        (case (grid:get pos)
          :# nil ; ignore rocks
          nil (do
                (local [x y] pos)
                (if (and (not min-north)
                         (< y 1))
                  (set min-north [pos steps]))
                (if (and (not min-south)
                         (> y grid.height))
                  (set min-south [pos steps]))
                (if (and (not min-west)
                         (< x 1))
                  (set min-west [pos steps]))
                (if (and (not min-east)
                         (> x grid.width))
                  (set min-east [pos steps])))
          _
          (let [s (sgrid:get pos)]
            (when (and (= false s)
                       (= (% steps 2) (% max 2)))
              ; (printv pos)
              (set count (+ count 1))
              (grid:set pos :O))
            (when (= false s)
              (set max-steps-used (math.max steps max-steps-used)))
            (when (or (= false s)
                      (< steps s))
              (sgrid:set pos steps)
              (when (< steps max)
                (table.insert remaining [(V.add pos V.north) (+ steps 1)])
                (table.insert remaining [(V.add pos V.south) (+ steps 1)])
                (table.insert remaining [(V.add pos V.west) (+ steps 1)])
                (table.insert remaining [(V.add pos V.east) (+ steps 1)]))))))
      (set current (table.remove remaining 1))))
  ; (if (< grid.width 12)
    ; (grid:print))
  ; (grid:print)
  (print "max steps" max-steps-used)
  (printv "min" min-north min-south min-west min-east)
  count)

(->> (read-lines)
     (map chars)
     grid2d.make
     count-steps
     printv)

;; The start is in the center
;; No rocks at the border
;; There are straight, rock-free lines from the center to all the edges (on the real input, not the example!)
