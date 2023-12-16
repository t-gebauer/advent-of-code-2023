(local {: read-lines : map : printv : sum} (require :lib))
(local re (require :re))

(local initialization-sequence-pattern (re.compile "{| ( {[^,]+} .? )+ |}"))

(λ ichars [text]
  (assert (= (type text) "string"))
  (local len (string.len text))
  (var i 0)
  (fn []
    (set i (+ i 1))
    (if (<= i len)
      (values i (text:sub i (+ i 1))))))

(λ HASH-algorithm [text]
  (accumulate [val 0
               _ c (ichars text)]
     (-> val
         (+ (string.byte c))
         (* 17)
         (% 256))))

(λ read-input []
  (let [lines (read-lines)]
    (assert (= 1 (# lines)))
    (. lines 1)))

(->> (read-input)
     (initialization-sequence-pattern:match)
     (map HASH-algorithm)
     sum
     printv)

