(local {: read-lines : match-numbers : map : printv : reduce : all : sum} (require :lib))

(λ differences [numbers]
  (local res [])
  (reduce (fn [a b]
            (table.insert res (- b a))
            b) numbers)
  res)

(λ all-zeros? [numbers]
  (all #(= 0 $1) numbers))

(λ prediction [numbers]
  (if (= (# numbers) 1)
    (error "not enough numbers to extrapolate"))
  (let [diffs (differences numbers)
        last-number (. numbers (# numbers))]
    (+ last-number
       (if (all-zeros? diffs)
         0
         (prediction diffs)))))

(->> (read-lines)
     (map match-numbers)
     (map prediction)
     sum
     printv)
