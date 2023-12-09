;;; Brute force does not work. Takes more than 10 minutes to reach
;;; 1_000_000_000 steps.
;;; It would have taken nearly 94 days to compute the solution.

(local fennel (require :fennel))
(local re (require :re))
(local {: read-lines : map : endless : all} (require :lib))

(λ parse-input []
  (let [lines (read-lines)
        [instr _ & nodes] lines
        instructions (re.match instr "{| {.}+ |}")
        nodes (map #(re.match $1 "{| {:id: .^3 :} ' = (' {:L: .^3 :} ', ' {:R: .^3 :} ')' |}") nodes)
        nodes (collect [_ {: id : L : R} (ipairs nodes)]
                id {: L : R})]
    {: instructions
     : nodes}))

(λ start? [id]
  (re.match id "..'A'"))

(λ end? [id]
  (re.match id "..'Z'"))

(λ find-the-end [{: nodes : instructions}]
  (var count 0)
  (var current (icollect [id _ (pairs nodes)]
                 (if (start? id)
                   id)))
  (print (fennel.view current))
  (each [instr (endless instructions) &until (or (all end? current)
                                                 (> count 999_999_999))]
    (set count (+ count 1))
    (set current (icollect [_ id (ipairs current)]
                   (. nodes id instr))))
  count)

(->> (parse-input)
     find-the-end
     fennel.view
     print)
