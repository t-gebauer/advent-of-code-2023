(local {: read-lines : map : printv : chars} (require :lib))
(local grid2d (require :grid2d))

(λ parse-input [lines]
  (local patterns [])
  (var current [])
  (table.insert patterns current)
  (each [_ line (ipairs lines)]
    (case line
      "" (do
           (set current [])
           (table.insert patterns current))
      _ (table.insert current (chars line))))
  (map grid2d.make patterns))

(λ is-reflection [pattern dir index]
  (let [size (. pattern (case dir
                          :horz :height
                          :vert :width))
        other-size (. pattern (case dir
                                :horz :width
                                :vert :height))
        make-pos #(case dir
                    :horz [$2 $1]
                    :vert [$1 $2])]
    (var found-smudge? false)
    (for [i index 1 -1 &until (or (< i 1)
                                  (> (+ index 1 (- index i)) size))]
      (for [j 1 other-size]
        (let [a (make-pos i j)
              b (make-pos (+ index 1 (- index i)) j)]
          (if (not= (pattern:get a)
                    (pattern:get b))
            (if found-smudge?
              (lua "return false")
              (set found-smudge? true))))))
    found-smudge?))

(λ find-reflection [pattern]
  (for [x 1 (- pattern.width 1)]
    (if (is-reflection pattern :vert x)
      (lua "return {'vert', x}")))
  (for [y 1 (- pattern.height 1)]
    (if (is-reflection pattern :horz y)
      (lua "return {'horz', y}"))))

(λ summarize [results]
  (accumulate [sum 0
               _ [dir i] (ipairs results)]
    (+ sum (case dir
             :horz (* 100 i)
             :vert i))))

(->> (read-lines)
     parse-input
     (map find-reflection)
     summarize
     printv)
