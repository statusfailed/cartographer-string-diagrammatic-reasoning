# Algebraic construction of Hypergraphs

Notes on the module `Data.Hypergraph.Algebraic`.

# Composition, `(|>)`, and `(<|)`.

We want "affine" compositions, which are a bit more convenient.
Consider composing the `2 → 2` and `3 → 1` diagrams below, to get a diagram of
type `3 → 1`.
In general, composing `a → b` with `c → d` gives
`a + min(0, c - b) → b + min(0, b - c)` (TODO: check this :)

    L----\  o------R    L------o
    L-----\----+---R    L----+----R
           ---/         L---/

    |
    v

    L----\  o-----------------o
    L-----\----+------------+----R
           ---/      ------/
                    /
    L--------------/


In the reverse case:

    L------o        L----\  o------R    
    L----+----R     L-----\----+---R    
    L---/                  ---/         


    L------o        
    L----+--------\  o------R    
    L---/      ----\----+---R    
              /     ---/         
    L--------/      

We can also have more nodes on the right-boundary of the left graph, so
composing a `2 → 2` graph with a `1 → 0` graph yields a `2 → 1` graph.

    L----\  o------R    L------o        
    L-----\----+---R
           ---/

    L----\  o--------o        
    L-----\----+--------R
           ---/


## Algorithm

`a |> b` is log-linear in the size of `b`, because we need to renumber all the
hyperedges of b, which means reconstructing the connection map.

How it works:

INPUT:
  a: a hypergraph, with nextHyperEdgeId = n
  b: a hypergraph

1. Renumber b
    - increment IDs of all hyperedges in b by n.
    - Increment IDs in connections by n (rebuild map)
2. Identify right boundary of a with left boundary of b.
    - for each wire in `(s, t) ∈ b`
    - if `s` is a `Boundary i`, look up the `ith` *RIGHT* boundary in `a`, and
      replace it with the corresponding source. If there is none, leave it
      unchanged.
    - (The idea is that if the right boundary of 'a' is larger, it'll still
      connect to the right boundary, and if it's smaller, then the left
      boundary of b will still connect to the left boundary.)
