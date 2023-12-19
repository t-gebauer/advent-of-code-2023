(local {: printv} (require :lib))
(local re (require :re))

(local workflow-pattern (re.compile "
  workflow <- {name} '{' {| (rule ',')+ |} {target} '}'
  rule <- {| condition ':' {:target:target:} |}
  condition <- {:cat:categorie:} {:op:[<>]:} {:value:%d+:}
  categorie <- 'x' / 'm' / 'a' / 's'
  target <- name / [AR]
  name <- [a-z]+
"))

(local parts-pattern (re.compile "
  '{x=' {%d+} ',m=' {%d+} ',a=' {%d+} ',s=' {%d+} '}'
"))

(λ read-input []
  (let [workflows {}
        parts []]
    (var parsing-workflows? true)
    (each [line (io.lines)]
      (if
        (= line "") (set parsing-workflows? false)
        (if parsing-workflows?
          (let [(name rules fallback) (workflow-pattern:match line)]
            (each [_ rule (ipairs rules)]
              (set rule.value (tonumber rule.value)))
            (table.insert rules {:target fallback})
            (tset workflows name rules))
          (let [(x m a s) (parts-pattern:match line)
                part {: x : m : a : s}]
            (each [k v (pairs part)]
              (tset part k (tonumber v)))
            (table.insert parts part)))))
    {: workflows : parts}))

(λ process-parts [{: workflows : parts}]
  (accumulate [rating 0
               _ part (ipairs parts)]
    (do
      (var workflow :in)
      (while (and (not= workflow :A)
                  (not= workflow :R))
        (local original-workflow workflow)
        (each [_ rule (ipairs (. workflows workflow))
               &until (not= workflow original-workflow)]
          (if (case rule.op
                :> (> (. part rule.cat) rule.value)
                :< (< (. part rule.cat) rule.value)
                nil true)
            (set workflow rule.target))))
      (if (= workflow :A)
        (+ rating part.x part.m part.a part.s)
        rating))))

(->> (read-input)
     process-parts
     printv)
