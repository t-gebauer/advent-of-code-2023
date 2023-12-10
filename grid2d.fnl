(local Grid {})

(λ Grid.get [self [x y]]
  "Ignores out-of-bound-indices"
  (?. self.data y x))

(λ Grid.set [self [x y] value]
  "Ignores out-of-bound-indices"
  (case (. self.data y)
    line (tset line x value)))

(λ Grid.find-one [self value]
  (for [y 1 self.height]
    (for [x 1 self.width]
      (let [pos [x y]]
        (if (= (self:get pos) value)
          (lua "return pos"))))))

(λ Grid.find-all [self value]
  (local found [])
  (for [y 1 self.height]
    (for [x 1 self.width]
      (let [pos [x y]]
        (if (= (self:get pos) value)
          (table.insert found pos)))))
  found)

(λ Grid.print [self]
  (each [_ line (ipairs self.data)]
    (print (table.concat line))))

(λ Grid.make [tbl]
  (let [height (# tbl)
        width (# (. tbl 1))
        data (icollect [_ line (ipairs tbl)]
               (icollect [_ char (ipairs line)]
                 char))]
    (setmetatable
     {: width
      : height
      :size [width height]
      : data}
     {:__index Grid})))

(λ Grid.make-size [[w h] initial]
  (let [height h
        width w
        data []]
    (for [_ 1 h]
      (let [row []]
        (for [_ 1 w]
          (table.insert row initial))
        (table.insert data row)))
    (setmetatable
     {: width
      : height
      :size [width height]
      : data}
     {:__index Grid})))

(setmetatable
 Grid
 {:__call #(Grid.make $2)})
