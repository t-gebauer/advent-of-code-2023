(local re (require :re))
(local fennel (require :fennel))
(local {: read-lines : map : copy} (require :lib))

;; This works only because the cards are sorted; equal cards are grouped together.
(local type-pattern (re.compile "
  type  <- five / four / full / three / twop / onep / high
  five  <- c =c^4 -> '7 five'
  four  <- ( c =c^3 -> '6 four' ) / . four
  full  <- c =c (=c {:o: . :} / {:o: . :} =o ) =o -> '5 full house'
  three <- ( c =c^2 -> '4 three' ) / . three
  twop  <- ( c =c (& onep) -> '3 two pairs' ) / . twop
  onep  <- ( c =c -> '2 one pair') / . onep
  high  <- '' -> '1 high card'
  c     <- {:c: . :}
"))

(λ number-of-card [card]
  (case card
    "A" 14
    "K" 13
    "Q" 12
    "J" 11
    "T" 10
    n (tonumber n)))

(λ parse-input [line]
  (let [(cards bid) (re.match line "{| {[^ ]}+ |} ' ' {[0-9]+}")
        numeric (map number-of-card cards)
        bid (tonumber bid)
        sorted (copy cards)
        _ (table.sort sorted)
        type (type-pattern:match (table.concat sorted))]
    {: cards : numeric : bid : type}))

(fn compare-numbers [a b]
  (each [i n (ipairs a)]
    (if
      (> n (. b i)) (lua "return true")
      (< n (. b i)) (lua "return false")))
  false)

(λ order-by-strength! [hands]
  (table.sort hands (fn [a b]
                      (if (= a.type b.type)
                        (compare-numbers a.numeric b.numeric)
                        (> a.type b.type))))
  hands)

(λ determine-winnings [hands]
  (let [number-of-hands (# hands)]
    (accumulate [winnings 0
                 i hand (ipairs hands)]
      (+ winnings (* (- number-of-hands i -1)
                     hand.bid)))))

(->> (read-lines)
     (map parse-input)
     order-by-strength!
     determine-winnings
     fennel.view
     print)

