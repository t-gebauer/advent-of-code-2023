(λ add [[ax ay az] [bx by bz]]
  [(+ ax bx)
   (+ ay by)
   (+ az bz)])

(λ sub [[ax ay az] [bx by bz]]
  [(- ax bx)
   (- ay by)
   (- az bz)])

(λ mul [[x y z] s]
  [(* s x)
   (* s y)
   (* s z)])

(λ div [[x y z] s]
  [(/ x s)
   (/ y s)
   (/ z s)])

(λ dot [[ax ay az] [bx by bz]]
  (+ (* ax bx)
     (* ay by)
     (* az bz)))

(λ cross [[ax ay az] [bx by bz]]
  [(- (* ay bz) (* az by))
   (- (* az bx) (* ax bz))
   (- (* ax by) (* ay bx))])

(λ zero? [[x y z]]
  (= 0 x y z))

(λ equal? [[ax ay az] [bx by bz]]
  (and
    (= ax bx)
    (= ay by)
    (= az bz)))

(λ len [[x y z]]
  (math.sqrt (+ (^ x 2)
                (^ y 2)
                (^ z 2))))

(λ norm [v]
  (div v (len v)))

(λ angle [a b]
  (math.acos
   (/ (dot a b)
      (* (len a) (len b)))))

{: add
 : sub
 : mul
 : div
 : dot
 : cross
 : zero?
 : equal?
 :length len
 : norm
 : angle}
