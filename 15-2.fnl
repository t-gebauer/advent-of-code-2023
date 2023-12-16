(local {: read-lines : printv} (require :lib))
(local re (require :re))

(local initialization-sequence-pattern
       (re.compile "{| ( {| {:label: [a-z]+ :} {:op: [-=] :} {:fl:[0-9]:}? |} ','? )+ |}"))

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

(λ make-sequence [len constructor]
  (local result [])
  (for [i 1 len]
    (tset result i (constructor)))
  result)

(λ table-find [tbl pred]
  (each [_i item (ipairs tbl)]
    (if (pred item)
      (lua "return _i"))))

(λ table-find-and-remove [tbl pred]
  (case (table-find tbl pred)
    index (table.remove tbl index)))

(λ table-replace-or-insert [tbl pred item]
  (case (table-find tbl pred)
    index (tset tbl index item)
    nil (table.insert tbl item)))

(λ HASHMAP [init-sequence]
  (let [boxes (make-sequence 256 #{})]
    (each [_ {: label : op : fl} (ipairs init-sequence)]
      (let [box-index (+ (HASH-algorithm label) 1)
            box (. boxes box-index)
            matches-label? (fn [[lbl _]] (= lbl label))]
        (case op
          :- (table-find-and-remove box matches-label?)
          := (table-replace-or-insert box matches-label? [label fl]))))
    boxes))

(λ sum-focusing-powers [boxes]
  (accumulate [sum 0
               i box (ipairs boxes)]
    (+ sum (accumulate [sum 0
                        j [_ fl] (ipairs box)]
             (+ sum (* i j fl))))))

(->> (read-input)
     (initialization-sequence-pattern:match)
     HASHMAP
     sum-focusing-powers
     printv)

