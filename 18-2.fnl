(local {: read-lines : map : printv} (require :lib))
(local V (require :vec2))
(local re (require :re))

(local pattern (re.compile "{| {[RLUD]} ' ' {%d+} ' (#' {[a-f%d]+} ')' |}"))

(λ direction [char]
  (case char
    :R V.right
    :L V.left
    :U V.up
    :D V.down))

(λ extract-instructions [line]
  (let [[ _ _ color] line
        hex (color:sub 1 5)
        len (tonumber (.. "0x" hex))
        d (color:sub 6 6)
        dir (case d
              :0 :R
              :1 :D
              :2 :L
              :3 :U)]
    [dir len]))

(λ calculate-size [plan]
  (var area 1)
  (var pos [1 1])
  (each [_ [d len] (ipairs plan)]
    (let [dir (direction d)
          next (V.add pos (V.mul dir len))]
      (if (or (= d :R)
              (= d :L))
        (let [[x y] pos
              [x2 _] next
              l (- x x2)
              a (* l y)]
          (set area (+ area a))))
      (set pos next))
    ; adjust the area for one of each [U D] and [R L]
    (if (or (= d :U)
            (= d :R))
      (set area (+ area len))))
  area)

(->> (read-lines)
     (map #(pattern:match $1))
     (map extract-instructions)
     calculate-size
     printv)
