(local fennel (require :fennel))
(local re (require :re))
(local {: read-lines : map : endless} (require :lib))

(λ parse-input []
  (let [lines (read-lines)
        [instr _ & nodes] lines
        instructions (re.match instr "{| {.}+ |}")
        nodes (map #(re.match $1 "{| {:id: .^3 :} ' = (' {:L: .^3 :} ', ' {:R: .^3 :} ')' |}") nodes)
        nodes (collect [_ {: id : L : R} (ipairs nodes)]
                id {: L : R})]
    {: instructions
     : nodes}))

(λ find-the-end [{: nodes : instructions}]
  (var count 0)
  (var current "AAA")
  (each [instr (endless instructions) &until (= current "ZZZ")]
    (set count (+ count 1))
    (set current (. nodes current instr)))
  count)

(->> (parse-input)
     find-the-end
     fennel.view
     print)
