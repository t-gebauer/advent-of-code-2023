(local fennel (require :fennel))
(local {: map : match-numbers : read-lines : zip : reduce} (require :lib))

(λ parse-input []
  (let [lines (read-lines)
        [times distances] (map match-numbers lines)
        races (zip times distances)]
    races))

(λ count-possible-ways-to-win [race]
  (var count 0)
  (let [[time record] race]
    (for [hold 1 time]
      (let [dist (* hold (- time hold))]
        (if (> dist record)
          (set count (+ count 1))))))
  count)

(->> (parse-input)
     (map count-possible-ways-to-win)
     (reduce #(* $1 $2))
     fennel.view
     print)
