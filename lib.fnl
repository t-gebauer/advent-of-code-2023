(local fennel (require :fennel))
(local re (require :re))

(λ map [fun list]
  (icollect [_ item (ipairs list)]
    (fun item)))

(λ zip [list other]
  (assert (= (# list) (# other)) "lengths must be equal")
  (icollect [i item (ipairs list)]
    [item (. other i)]))

(λ match-numbers [line]
  (->> [(re.match line "({'-'?[0-9]+} / .)+")]
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

(λ endless [list]
  "An iterator endlessly returning the next element from a list."
  (local l (# list))
  (var i 0)
  (λ []
    (set i (if (= i l)
             1
             (+ i 1)))
    (. list i)))

(λ endless-i [list]
  "Same as endless, but also returns the index in the original list."
  (local l (# list))
  (var i 0)
  (λ []
    (set i (if (= i l)
             1
             (+ i 1)))
    (values i (. list i))))

(λ all [pred list]
  (each [_ node (pairs list)]
    (if (not (pred node))
      (lua "return false")))
  true)

(λ printv [& args]
  "Warning: stops at nil!"
  (print (unpack (map fennel.view args))))

(λ tset-nested [tbl keys v]
  "A setter equivalent to the ?. getter."
  (accumulate [t tbl
               i key (ipairs keys)]
    (if (= i (# keys))
      (tset t key v)
      (let [next (or (. t key) {})]
        (tset t key next)
        next))))

(λ gcd [a b]
  "Greatest common divisor; using the Euclidean algorithm"
  (if (= b 0)
    a
    (gcd b (% a b))))

(λ lcm [a b]
  "Least common multiple"
  (* a (/ b (gcd a b))))

{: map
 : match-numbers
 : read-lines
 : sum
 : zip
 : reduce
 : copy
 : endless
 : endless-i
 : all
 : printv
 : tset-nested
 : gcd
 : lcm}
