(local re (require :re))
(local {: read-lines : map : endless-i : tset-nested : printv : reduce : lcm} (require :lib))

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

(λ find-endings [{: nodes : instructions} start]
  (var steps 0)
  (var current start)
  (local endings [])
  (local first-visited {})
  (local visited {})
  (each [i instr (endless-i instructions) &until (?. visited current i)]
    (if (not (. first-visited current))
      (tset first-visited current steps))
    (tset-nested visited [current i] steps)
    (set steps (+ steps 1))
    (set current (. nodes current instr))
    (if (end? current)
      (table.insert endings {: steps :instr-index i})))
  {: endings
   :loop-entered-at (. first-visited current)
   :loop-left-at steps
   :loop-length (- steps (. first-visited current))})

(λ find-the-end [{: nodes &as the-map}]
  (local starts (icollect [id _ (pairs nodes)]
                  (if (start? id)
                    id)))
  (->> starts
       (map #(find-endings the-map $1))
       ;; This only works, because
       ;; - the loop length equals the initial step count to the ending; offsets
       ;;   can be ignored
       ;; - there is only one ending for each start in the real input
       ;; - all endings are on the same instruction-index, otherwise they might
       ;;   never be reached simultaneously; we would have to consider multiple
       ;;   different offset (with the same loop length)
       ;; The solution is simply the least common multiple of all endings.
       (map #(. $1 :endings 1 :steps))
       (reduce lcm)))

(->> (parse-input)
     find-the-end
     printv)
