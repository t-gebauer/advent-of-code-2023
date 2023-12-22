(local {: read-lines : map : printv : table-keys} (require :lib))
(local re (require :re))

(local pattern (re.compile "
  line     <- {| {:start:position:} '~' {:end:position:} |}
  position <- {| num ',' num ',' num |}
  num      <- {%d+} -> tonumber
" {: tonumber}))

;; Took me a while to notice that the bricks in the real input are not sorted
(λ sort-by-high [bricks]
  (table.sort bricks (fn [a b]
                       (let [[_ _ az] a.start
                             [_ _ bz] b.start]
                         (< az bz))))
  bricks)

(λ index [x y]
  (.. x "-" y))

(λ stack-bricks [bricks]
  (local lower-bricks {})
  (local higher-bricks {})
  (for [i 1 (# bricks)]
    (tset higher-bricks i []))
  (local max-z-for-pos {})
  (each [brick-index {:start [sx sy sz]
                      :end [ex ey ez]} (ipairs bricks)]
    (assert (<= sx ex))
    (assert (<= sy ey))
    (assert (<= sz ez))
    (var max-z 0)
    (var lower-bricks* {})
    (for [x sx ex]
      (for [y sy ey]
        (let [i (index x y)]
          (case (. max-z-for-pos i)
            [z b] (if
                    (> z max-z)
                    (do
                      (set max-z z)
                      (set lower-bricks* {b true}))
                    (= z max-z)
                    (do
                      (tset lower-bricks* b true)))))))
    (let [high (+ (- ez sz) 1)
          new-max (+ max-z high)]
      (for [x sx ex]
        (for [y sy ey]
          (let [i (index x y)]
            (tset max-z-for-pos i [new-max brick-index])))))
    (tset lower-bricks brick-index (table-keys lower-bricks*))
    (each [b _ (pairs lower-bricks*)]
      (table.insert (. higher-bricks b) brick-index)))
  {: lower-bricks
   : higher-bricks})

(λ count-non-essential-bricks [{: lower-bricks
                                : higher-bricks}]
  (var count 0)
  (each [_ higher (ipairs higher-bricks)]
    (var safe-to-disintegrate? true)
    (each [_ b (ipairs higher) &until (not safe-to-disintegrate?)]
      (if (= (# (. lower-bricks b)) 1)
        (set safe-to-disintegrate? false)))
    (if safe-to-disintegrate?
      (set count (+ count 1))))
  count)

(->> (read-lines)
     (map #(pattern:match $1))
     sort-by-high
     stack-bricks
     count-non-essential-bricks
     printv)
