(local {: read-lines : map : printv : sum} (require :lib))
(local re (require :re))
(local {: P} (require :lpeg))

(local number-pattern (re.compile "{| [.#?]+ ' ' ({ %d+ } ','?)+ |}"))
(local spring-char-pattern (re.compile "{| {[.#?]}+ |}"))


(λ exactly [pattern len]
  "matches pattern exactly len times"
  (var p (P pattern))
  (for [_ 2 len]
    (set p (* p (P pattern))))
  p)

(λ block-pattern [len]
  "matches a block of # with length len"
  (* (+ (^ (P ".") 0))
     (exactly "#" len)
     (+ (P ".") -1)))

(λ make-pattern [numbers]
  "matches a string only if it contains block-patterns with the length of each given number"
  (var p (P true))
  (each [_ num (ipairs numbers)]
    (set p (* p (block-pattern num))))
  (* p
     (^ (P ".") 0)
     -1 ; end of line
     ))

(λ count-possible-arrangements [line]
  (let [numbers (->> line
                     (number-pattern:match)
                     (map tonumber))
        springs (spring-char-pattern:match line)]

    (var count 0)

    (local pattern (make-pattern numbers))
    (λ check []
      (if (not= nil (pattern:match (table.concat springs)))
        (set count (+ count 1))))

    (local unknowns [])
    (each [i c (ipairs springs)]
      (if (= c :?)
        (table.insert unknowns i)))

    (λ check-arrangement [i]
      (if (<= i (# unknowns))
        (do
          (tset springs (. unknowns i) :.)
          (check-arrangement (+ i 1))
          (tset springs (. unknowns i) :#)
          (check-arrangement (+ i 1)))
        (check)))

    (check-arrangement 1)

    count))

(->> (read-lines)
     (map count-possible-arrangements)
     sum
     printv)
