;;; Version b: Without Parsing Expression Grammars (PEG)

(local is-part-1 (= nil (. arg 1)))

(λ string-split [str sep]
  (if (not= (length sep) 1)
    (error "separator must be a single char"))
  (icollect [part (string.gmatch str (.. "([^" sep "]+)"))]
    part))

(λ split-line [line]
  (let [[head body] (string-split line ":")
        [_ game] (string-split head " ")
        sets (string-split body ";")
        sets (icollect [_ set_ (ipairs sets)]
               (let [colors (string-split set_ ",")]
                 (icollect [_ color (ipairs colors)]
                   (string-split color " "))))]
    {: game : sets}))

(print (accumulate [acc 0
                    line (io.lines)]
         (let [{: game : sets} (split-line line)
               max {}]
           (each [_ set_ (ipairs sets)]
             (each [_ [num color] (ipairs set_)]
               (tset max color (math.max num (or (. max color) 0)))))
           (+ acc
              (if is-part-1
                ;; part 1
                (if (and (<= (. max :red) 12)
                         (<= (. max :green) 13)
                         (<= (. max :blue) 14))
                  game
                  0)
                ;; part 2
                (* (. max :red)
                   (. max :green)
                   (. max :blue)))))))
