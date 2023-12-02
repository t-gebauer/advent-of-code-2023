(λ isdigit [char]
  (case char
    (where (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9")) true
    _ false))

(λ number-of-line [line]
  (var first nil)
  (var last nil)
  (each [char (line:gmatch ".")]
    (if (isdigit char)
      (if (= nil first)
        (set first char)
        (set last char))))
  (if (= nil first)
    (error "missing a number"))
  (assert (tonumber (.. first (or last first)))))

(print (accumulate [sum 0
                    line (io.lines)]
         (+ sum (number-of-line line))))
