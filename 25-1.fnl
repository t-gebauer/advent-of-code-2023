;;; Try all combinations. Too slow for the real input.

(local {: read-lines : map : printv : table-keys : table-values : any} (require :lib))
(local re (require :re))

(local pattern (re.compile "
  line      <- {| component ':' {| (' ' component)+  |} |}
  component <- {[a-z]+}
"))

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

(λ find-splitting-edges [nodes]
  (local node-count (# (table-keys nodes)))
  (local edges* {})
  (each [from to* (pairs nodes)]
    (each [to _ (pairs to*)]
      (tset edges* (edge-index from to) [from to])))
  (local edges (table-values edges*))
  (local edge-count (# edges))
  (for [i 1 (- edge-count 2)]
    (for [j (+ i 1) (- edge-count 1)]
      (for [k (+ j 1) edge-count]
        (let [[start _] (. edges k)
              count1 (count-reachable-nodes nodes [(. edges i) (. edges j) (. edges k)] start)]
          (if (< count1 node-count)
            (let [count2 (- node-count count1)
                  _result [(. edges i) (. edges j) (. edges k)]]
              (printv "split into two groups:" count1 count2 "=>" (* count1 count2))
              (lua "return _result"))))))))

(->> (read-input)
     (find-splitting-edges)
     printv)
