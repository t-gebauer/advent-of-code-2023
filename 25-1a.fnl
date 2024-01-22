;;; Take away an edge, find the shortest remaining path between its two nodes,
;;; then take edges out of that path. Repeat twice.
;;; We have found the solution if there is no remaining path between the nodes
;;; of the last edge.

(local {: read-lines : map : printv : table-keys : any} (require :lib))
(local re (require :re))

(local pattern (re.compile "
  line      <- {| component ':' {| (' ' component)+ |} |}
  component <- {[a-z]+}
" {: tonumber}))

(λ read-input []
  (let [lines (read-lines)
        input (map #(pattern:match $1) lines)
        nodes {}]
    (λ insert-edge [from to]
      (if (= nil (. nodes from))
        (tset nodes from {}))
      (tset (. nodes from) to true))
    (each [_ [from to] (ipairs input)]
      (each [_ to (ipairs to)]
        (insert-edge from to)
        (insert-edge to from)))
    nodes))

(λ edge-index [a b]
  (if (< a b)
    (.. a "/" b)
    (.. b "/" a)))

(λ edge-equals? [[a b] [c d]]
  (or (and (= a c)
           (= b d))
      (and (= a d)
           (= b c))))

(λ count-reachable-nodes [nodes blocked-edges start]
  (λ is-blocked? [edge]
    (any #(edge-equals? $1 edge) blocked-edges))
  (var count 0)
  (let [nodes-left [start]
        visited {}]
    (while (> (# nodes-left) 0)
      (local current (table.remove nodes-left))
      (when (not (. visited current))
        (tset visited current true)
        (set count (+ count 1))
        (each [other _ (pairs (. nodes current))]
          (if (and (not (. visited other))
                   (not (is-blocked? [current other])))
            (table.insert nodes-left other)))))
    count))

(λ is-blocked? [blocked-edges edge]
  (any #(edge-equals? $1 edge) blocked-edges))

(λ build-path [nodes blocked-egdes steps-needed start end]
  (local path [start])
  (var current start)
  (var current-steps (- (. steps-needed start) 1))
  (while (not= current end)
    (var next nil)
    (each [other _ (pairs (. nodes current))]
      (when (and (not (is-blocked? blocked-egdes [current other]))
                 (= (. steps-needed other) current-steps))
        (set current-steps (- current-steps 1))
        (set next other)
        (table.insert path other)))
    (if (not next)
      (error "path does not exist")
      (set current next)))
  path)

(λ find-shortest-path [nodes blocked-edges start end]
  (let [steps-needed {start 0}
        remaining [[start 1]]]
    (while (> (# remaining) 0)
      (local [node steps] (table.remove remaining 1))
      (each [other _ (pairs (. nodes node))]
        (when (and (not (. steps-needed other))
                   (not (is-blocked? blocked-edges [node other])))
          (tset steps-needed other steps)
          (if (= other end)
            (let [_path (build-path nodes blocked-edges steps-needed end start)]
              (lua "return _path"))
            (table.insert remaining [other (+ steps 1)])))))))

(λ all-edges [path]
  (var i 0)
  (λ []
    (set i (+ i 1))
    (if (< i (# path))
      [(. path i) (. path (+ i 1))])))

(λ find-splitting-edges [nodes]
  (local node-count (# (table-keys nodes)))
  (local edges {})
  (each [from to* (pairs nodes)]
    (each [to _ (pairs to*)]
      (tset edges (edge-index from to) [from to])))
  (each [_ edge1 (pairs edges)]
    (let [[start1 end1] edge1
          path1 (find-shortest-path nodes [edge1] start1 end1)]
      (printv "a:    " start1 end1 path1)
      (each [edge2 (all-edges path1)]
        (let [[start2 end2] edge2
              path2 (find-shortest-path nodes [edge1 edge2] start2 end2)]
          (each [edge3 (all-edges path2)]
            (let [[start3 end3] edge3
                  path3 (find-shortest-path nodes [edge1 edge2 edge3] start3 end3)]
              (when (not path3)
                (printv "found:" edge1 edge2 edge3)
                (let [count (count-reachable-nodes nodes [edge1 edge2 edge3] start3)
                      other-count (- node-count count)]
                  (printv count other-count (* count other-count)))
                (lua "return")))))))))

(->> (read-input)
     (find-splitting-edges)
     printv)
