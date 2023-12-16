(local {: read-lines : map : printv : chars} (require :lib))
(local grid2d (require :grid2d))

(macro inc [name value]
  `(set ,name (+ ,name ,value)))

(λ calculate-north-suppor-beam-load [platform]
  (var load 0)
  (for [x 1 platform.width]
    (var stable-pos 0)
    (for [y 1 platform.height]
      (let [shape (platform:get [x y])]
        (case shape
          :O (do
               (inc stable-pos 1)
               (inc load (- platform.height stable-pos -1)))
          :# (set stable-pos y)))))
  load)

(->> (read-lines)
     (map chars)
     grid2d.make
     calculate-north-suppor-beam-load
     printv)
