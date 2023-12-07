(local re (require :re))
(local fennel (require :fennel))
(local {: read-lines : map : copy} (require :lib))

;; This works only because the cards are sorted; equal cards are grouped together.
(local type-pattern (re.compile "
  type  <- five / four / full / three / twop / onep / high
  five  <- ( FIVE / &j1 FOUR / &j2 &!j3 THREE / &j2 FULL ) -> '7 five'
  FIVE  <- c =c^4
  four  <- ( FOUR / &j1 THREE / &j2 TWOP / &j3 ) -> '6 four'
  FOUR  <- c =c^3 / . FOUR
  full  <- ( FULL / &j1 TWOP ) -> '5 full house'
  FULL  <- c =c (=c {:o: . :} / {:o: . :} =o ) =o
  three <- ( THREE / &j1 ONEP / &j2 ) -> '4 three'
  THREE <- c =c^2 / . THREE
  twop  <- (TWOP) -> '3 two pairs'
  TWOP  <- c =c (& ONEP) / . TWOP
  onep  <- ( ONEP / &j1 ) -> '2 one pair'
  ONEP  <- c =c / . ONEP
  high  <- '' -> '1 high card'
  c  <- {:c: . :}
  j1 <- 'J' / . j1
  j2 <- 'J' j1 / . j2
  j3 <- 'J' j2 / . j3
"))

(λ number-of-card [card]
  (case card
    "A" 14
    "K" 13
    "Q" 12
    "J" 1
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

