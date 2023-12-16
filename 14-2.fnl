(local {: read-lines : map : printv : chars} (require :lib))
(local grid2d (require :grid2d))

(macro inc [name value]
  `(set ,name (+ ,name ,value)))

(λ calculate-north-support-beam-load [platform]
  (var load 0)
  (for [x 1 platform.width]
    (for [y 1 platform.height]
      (let [shape (platform:get [x y])]
        (case shape
          :O (inc load (- platform.height y -1))))))
  load)

(λ tilt-platform [platform dir]
  ;; This only works because width and height are equal
  (let [make-pos #(case dir
                    :north [$1 $2]
                    :west [$2 $1]
                    :south [$1 (- platform.height $2 -1)]
                    :east [(- platform.width $2 -1) $1])]
    (for [x 1 platform.width]
      (var stable-pos 0)
      (for [y 1 platform.height]
        (let [shape (platform:get (make-pos x y))]
          (case shape
            :O (do
                 (inc stable-pos 1)
                 (when (not= stable-pos y)
                   (platform:set (make-pos x stable-pos) :O)
                   (platform:set (make-pos x y) :.)))
            :# (set stable-pos y)))))))

(λ simple-cycle-detection [numbers]
  (let [len (# numbers)
        last (. numbers len)]
    (var cycle nil)
    (for [i (- len 1) 1 -1 &until cycle]
      (if (= last (. numbers i))
        (let [cycle-candidate (- len i)]
          (print "cycle candidate at" i "length" (- len i))
          (var is-cycle? true)
          (for [j 2 5]
            (if (not= last (. numbers (- len (* j cycle-candidate))))
              (set is-cycle? false)))
          (if is-cycle?
            (set cycle cycle-candidate)))))
    cycle))

(λ calculate-north-support-beam-load-after-tilting [platform]
  (assert (= platform.width platform.height))
  (local directions [:north :west :south :east])
  (local cycle-results [])
  (for [_ 1 1000]
    (each [_ direction (ipairs directions)]
      (tilt-platform platform direction))
    (table.insert cycle-results (calculate-north-support-beam-load platform)))
  (let [cycle (simple-cycle-detection cycle-results)]
    (let [z 800]
      (. cycle-results (- z (- (% z cycle)
                               (% 1_000_000_000 cycle)))))))

(->> (read-lines)
     (map chars)
     grid2d.make
     calculate-north-support-beam-load-after-tilting
     printv)
