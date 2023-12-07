(local re (require :re))

(λ map [fun list]
  (icollect [_ item (ipairs list)]
    (fun item)))

(λ zip [list other]
  (assert (= (# list) (# other)) "lengths must be equal")
  (icollect [i item (ipairs list)]
    [item (. other i)]))

(λ match-numbers [line]
  (->> [(re.match line "({[0-9]+} / .)+")]
       (map tonumber)))

(λ read-lines []
  (icollect [line (io.lines)]
    line))

(λ sum [list]
  (accumulate [sum 0 _ n (ipairs list)]
    (+ sum n)))

(λ reduce [fun list]
  "a left fold"
  (accumulate [acc nil
               _ v (ipairs list)]
    (if (= nil acc)
      v
      (fun acc v))))

(λ copy [t]
   (collect [k v (pairs t)]
     k v))

{: map
 : match-numbers
 : read-lines
 : sum
 : zip
 : reduce
 : copy}
