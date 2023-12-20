;;; Not fully automated ...

(local {: read-lines : map : printv : table-keys : table-values : all} (require :lib))
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
        nil (tset modules out {:name out :outputs []}))))
  ;; Find inputs for :rx
  (each [_ mod (pairs modules)]
    (each [_ out (ipairs mod.outputs)]
      (when (= out :rx)
        (set mod.rx-input true)
        ;; ... it is just a single conjunction module: gq
        ;; Now, if we list its inputs ...
        (printv (table-keys mod.inputs))))))

(λ process-pulses [modules iteration]
  (let [pulses [[:button false :broadcaster]]]
    (while (> (# pulses) 0)
      (local [source pulse target] (table.remove pulses 1))
      (local mod (. modules target))
      ;; ... and find out in which intervals they send high pulses ...
      (if (and mod.rx-input
               pulse)
        (print mod.name "high input" source iteration))
      (let [reaction (case mod.type
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
            (table.insert pulses [mod.name reaction other])))))))

(let [modules (read-input)]
  (initialize-modules modules)
  (for [i 1 10000]
    (process-pulses modules i)))

;; ... we can multiple them to find the answer:
(print (string.format "%0.f" (* 3733 3911 4019 4093)))
