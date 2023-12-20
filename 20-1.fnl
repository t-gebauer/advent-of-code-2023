(local {: read-lines : map : printv : table-values : all} (require :lib))
(local re (require :re))

(local pattern (re.compile "
  line    <- {| id ' -> ' {:outputs: {| outputs |} :} |}
  id      <- {:type:[&%]:}?{:name:[a-z]+:}
  outputs <- ({[a-z]+} ', '?)+
"))

(λ read-input []
  (let [modules {}
        lines (read-lines)
        mods (map #(pattern:match $1) lines)]
    (each [_ mod (ipairs mods)]
      (if (and (= nil mod.type)
               (= mod.name "broadcaster"))
        (set mod.type mod.name))
      (tset modules mod.name mod))
    modules))

(λ initialize-modules [modules]
  ;; Flip-flop
  (each [_ mod (pairs modules)]
    (if (= mod.type :%)
      (set mod.on false)))
  ;; Conjunction
  (each [_ mod (pairs modules)]
    (each [_ out (ipairs mod.outputs)]
      (case (. modules out)
        target (when (= target.type :&)
                 (if (= nil target.inputs)
                   (set target.inputs {}))
                 (tset target.inputs mod.name false)))))
  ;; Untyped modules
  (each [_ mod (pairs modules)]
    (each [_ out (ipairs mod.outputs)]
      (case (. modules out)
        nil (tset modules out {:name out :outputs []})))))

(λ process-pulses [modules]
  (var low-count 0)
  (var high-count 0)
  (let [pulses [[:button false :broadcaster]]]
    (while (> (# pulses) 0)
      (local [source pulse target] (table.remove pulses 1))
      (if pulse
        (set high-count (+ high-count 1))
        (set low-count (+ low-count 1)))
      (let [mod (. modules target)
            reaction (case mod.type
                       :% (case pulse
                            false (do
                                    (set mod.on (not mod.on))
                                    mod.on))
                       :& (do
                            (tset mod.inputs source pulse)
                            (not (all #(= true $1) (table-values mod.inputs))))
                       :broadcaster pulse)]
        (if (not= nil reaction)
          (each [_ other (ipairs mod.outputs)]
            (table.insert pulses [mod.name reaction other]))))))
  (values low-count high-count))

(let [modules (read-input)]
  (initialize-modules modules)
  (var sum-low 0)
  (var sum-high 0)
  (for [_ 1 1000]
    (let [(low high) (process-pulses modules)]
      (set sum-low (+ sum-low low))
      (set sum-high (+ sum-high high))))
  (printv (* sum-low sum-high)))
