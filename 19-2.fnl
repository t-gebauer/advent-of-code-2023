(local re (require :re))

(local workflow-pattern (re.compile "
  workflow <- {name} '{' {| (rule ',')+ |} {target} '}'
  rule <- {| condition ':' {:target:target:} |}
  condition <- {:cat:categorie:} {:op:[<>]:} {:value:%d+:}
  categorie <- 'x' / 'm' / 'a' / 's'
  target <- name / [AR]
  name <- [a-z]+
"))

(λ read-input []
  (let [workflows {}]
    (var done? false)
    (each [line (io.lines) &until done?]
      (if (= line "")
        (set done? true)
        (let [(name rules fallback) (workflow-pattern:match line)]
          (each [_ rule (ipairs rules)]
            (set rule.value (tonumber rule.value)))
          (table.insert rules {:target fallback})
          (tset workflows name rules))))
    workflows))

(λ copy [[min max]]
  [min max])

(λ part-matches-rule? [rule p]
  (if (= nil rule.op)
    true
    (let [[min max] (. p rule.cat)]
      (if
        (or (and (= rule.op :>)
                 (< rule.value min))
            (and (= rule.op :<)
                 (> rule.value max))) true ; full match
        (or (and (= rule.op :>)
                 (> rule.value max))
            (and (= rule.op :<)
                 (< rule.value min))) false ; no match
        :else ; partial match
        (do
          ;; create new part
          (let [newp {:flow rule.target
                      :x (copy p.x)
                      :m (copy p.m)
                      :a (copy p.a)
                      :s (copy p.s)}]
            (tset newp rule.cat (case rule.op
                                  :< [min (- rule.value 1)]
                                  :> [(+ rule.value 1) max]))
            ;; adjust current part
            (tset p rule.cat (case rule.op
                               :< [rule.value max]
                               :> [min rule.value]))
            ;; return new part
            newp))))))

(λ diff [[min max]]
  (- max min -1))

(λ find-accepted-combinations [worflows]
  (var accepted 0)
  (let [remaining [{:flow :in
                    :x [1 4000]
                    :m [1 4000]
                    :a [1 4000]
                    :s [1 4000]}]]
    (while (> (# remaining) 0)
      (let [p (table.remove remaining)]
        (case p.flow
          :A (set accepted (+ accepted (* (diff p.x)
                                          (diff p.m)
                                          (diff p.a)
                                          (diff p.s))))
          :R nil
          _
          (do
            (var break? false)
            (each [_ rule (ipairs (. worflows p.flow)) &until break?]
              (case (part-matches-rule? rule p)
                ;; complete match -> follow, unmodified
                true (do
                       (set p.flow rule.target)
                       (table.insert remaining p)
                       (set break? true))
                ;; no match -> continue, unmodified
                false nil
                ;; partial match -> split: both follow and continue
                new (table.insert remaining new)))))))
    accepted))

(->> (read-input)
     find-accepted-combinations
     (string.format "%.0f") ; otherwise it would be abbreviated as 1.…e+14
     print)
