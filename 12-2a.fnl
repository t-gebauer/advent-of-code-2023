;;; Better than brute-force, fast enough for the part2 example, but still too slow
;;; for some of the unfolded inputs.

(local {: read-lines : map : printv : sum} (require :lib))
(local re (require :re))

(local is-part-2? (= :2 (or (. arg 1) :1)))

(local number-pattern (re.compile "{| [.#?]+ ' ' ({ %d+ } ','?)+ |}"))
(local spring-char-pattern (re.compile "{| {[.#?]}+ |}"))

(λ count-possible-arrangements [line]
  (let [numbers (->> line
                     (number-pattern:match)
                     (map tonumber))
        springs (->> line
                     (spring-char-pattern:match))]
    (print line)

    (when is-part-2?
      (local num-numbers (# numbers))
      (local num-springs (# springs))
      (for [_ 1 4]
        (for [i 1 num-numbers]
          (table.insert numbers (. numbers i)))
        (table.insert springs :?)
        (for [i 1 num-springs]
          (table.insert springs (. springs i)))))

    (table.insert springs 1 :.)
    (table.insert springs :.)

    (var last-block 0)
    (for [i (# springs) 1 -1 &until (> last-block 0)]
      (case (. springs i)
        :# (set last-block i)))

    (λ count-matches [start numbers]
      (let [num (. numbers 1)
            remaining (icollect [i n (ipairs numbers)]
                        (if (> i 1) n))
            min-further-space (accumulate [sum -1
                                           _ n (ipairs numbers)]
                                (+ sum 1 n))]
        (var count 0)
        (var first-block nil)
        (for [i start (- (# springs) min-further-space)
              &until (and (not= nil first-block)
                          (> i (+ first-block 0)))]
          (var matching true)
          (if (= :# (. springs (- i 1)))
            (set matching false)
            (= :# (. springs (+ i num)))
            (set matching false))
          (for [j i (+ i num -1) &until (and (not matching)
                                             (not= nil first-block))]
            (if (= :. (. springs j))
              (set matching false))
            (if (and (= :# (. springs j))
                     (= nil first-block))
              (set first-block j)))
          (if matching
            (do
              (set count (+ count (if (> (# remaining) 0)
                                    (count-matches (+ i num 1) remaining)
                                    (if (> (+ i num) last-block)
                                      1
                                      0)))))))
        count))

    (let [res (count-matches 2 numbers)]
      (print res)
      res)))

(->> (read-lines)
     (map count-possible-arrangements)
     sum
     printv)
