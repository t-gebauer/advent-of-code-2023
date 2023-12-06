(local fennel (require :fennel))
(local {: map : match-numbers : read-lines} (require :lib))

(λ parse-input []
  (->> (read-lines)
       (map match-numbers)
       (map table.concat)
       (map tonumber)))

;; Slightly improved version of 06-1 which derives the maximum hold time from
;; the minimum hold time. Anyway... that is just a neglectable 3x speed up.
(λ count-possible-ways-to-win [race]
  (let [[time record] race]
    (for [hold 1 time]
      (let [dist (* hold (- time hold))]
        (if (> dist record)
          (let [max-hold (- time hold)
                _count (- max-hold hold -1)]
            (lua "return _count")))))))

(->> (parse-input)
     count-possible-ways-to-win
     fennel.view
     print)
