(local Grid {})

(λ Grid.get [self [x y]]
  "Ignores out-of-bound-indices"
  (?. self.data y x))

(λ Grid.set [self [x y] value]
  "Ignores out-of-bound-indices"
  (case (. self.data y)
    line (tset line x value)))

(λ Grid.force-set [self [x y] value]
  (case (. self.data y)
    line (tset line x value)
    nil (let [new []]
          (tset new x value)
          (tset self.data y new)
          (if (> x self.width)
            (set self.width x))
          (if (> y self.height)
            (set self.height y))
          (if (< x 1)
            (set self.negx x))
          (if (< y 1)
            (set self.negy y))
          (set self.size [self.width self.height]))))

(λ Grid.find-one* [self pred]
  (for [y 1 self.height]
    (for [x 1 self.width]
      (let [pos [x y]]
        (if (pred (self:get pos))
          (lua "return pos"))))))

(λ Grid.find-one [self value]
  (Grid.find-one* self #(= $1 value)))

(λ Grid.find-all* [self pred]
  (local found [])
  (for [y 1 self.height]
    (for [x 1 self.width]
      (let [pos [x y]]
        (if (pred (self:get pos))
          (table.insert found pos)))))
  found)

(λ Grid.find-all [self value]
  (Grid.find-all* self #(= $1 value)))

(λ Grid.print [self]
  (each [_ line (ipairs self.data)]
    (print (table.concat line))))

(λ Grid.print-size [self ?size]
  (let [[w h] (or ?size self.size)]
    (for [y 1 h]
      (print (faccumulate [line ""
                           x 1 w]
               (.. line (or (self:get [x y]) ".")))))))

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

(λ Grid.make-size* [[w h] constructor]
  (let [height h
        width w
        data []]
    (for [_ 1 h]
      (let [row []]
        (for [_ 1 w]
          (table.insert row (constructor)))
        (table.insert data row)))
    (setmetatable
     {: width
      : height
      :size [width height]
      : data}
     {:__index Grid})))

(λ Grid.make-size [size initial]
  (Grid.make-size* size (fn [] initial)))

(setmetatable
 Grid
 {:__call #(Grid.make $2)})
