(local lpeg (require :lpeg))

(local P lpeg.P)
(local C lpeg.C)
(local V lpeg.V)

;; Fennel does not have `apply`, only Lua's `unpack`, but that does not work
;; with operators, because their argument count needs to be known at compile time.
;; We could just use the functions used for overloading the operators.
;; (local lpeg-meta (getmetatable (P true)))
;; But then we still can not unpack into them, because they only accept two arguments.
;; We also need a function to recursively apply to a binary operator.
(λ apply [binary-op list]
  (accumulate [acc nil
               _ v (ipairs list)]
    (if (= nil acc)
      v
      (binary-op acc v))))

(λ map [fun list]
  (icollect [_ v (ipairs list)]
    (fun v)))

;; From the docs "Searching for a pattern": http://www.inf.puc-rio.br/~roberto/lpeg/lpeg.html#ex
(λ anywhere [p]
  ;; A grammar (`[]`) with one rule,
  ;; which either(`+`) matches the pattern(`p`)
  ;; or matches one character(`1`) and(`*`) recursively the first rule `(V 1)`
  (P [(+ p (* 1 (V 1)))]))

(λ make-number-pattern [strings]
  (->> strings
       (map C)
       (apply #(+ $1 $2))
       (anywhere)))

(local number-strings [:one :two :three :four :five :six :seven :eight :nine
                       :1 :2 :3 :4 :5 :6 :7 :8 :9])
(local forward-pattern (make-number-pattern number-strings))
(local backward-pattern (make-number-pattern (map #($1:reverse) number-strings)))

(local numbers-of-strings (collect [i num (ipairs number-strings)]
                            num i))

(λ number-of-string [string]
  (or (tonumber string)
      (. numbers-of-strings string)))

(λ number-of-line [line]
  (let [first (forward-pattern:match line)
        last (backward-pattern:match (line:reverse))]
    (if (or (= nil first)
            (= nil last))
      (error (.. "missing match on line: " line)))
    (assert (tonumber (.. (number-of-string first)
                          (number-of-string (last:reverse)))))))

(print (accumulate [sum 0
                    line (io.lines)]
         (+ sum (number-of-line line))))
