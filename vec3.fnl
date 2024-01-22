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

{: dot
 : cross
 : zero?}
