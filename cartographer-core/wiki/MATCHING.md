# Matching

Matching in cartographer is slow in some cases.
For example, this is fast:

    match a (a → b)

But this is (very) slow:

    match b (a → b)

This is very dependent on the order which nodes are "matched". Doing a
depth-first-search is much slower than just arbitrarily listing the connections
(for some reason!)

# Fully-connected patterns

A fully connected hypergraph is one where every node is reachable from every
other node in the underlying undirected hypergraph.

However, in general patterns are not fully connected, e.g.:

    L---o  o---R

This means every time we start on a new connected component, there is
nondeterminism: we have a large se of candidate nodes to try to match for a
given starting node: it is not fully determined at all by the previous
connected component.

# Indexing

One idea to speed things up: Indexing nodes.

    index :: Map (sig, Int, Int, sig) (Wire Open)

Here, we can look up nodes by the types of hyperedge they are "attached" to,
without caring about the hyperedge IDs.
Essentially, we just forget the hyperedge IDs.

However, this doesn't work for boundary nodes.
The problem is that an Li or Rj port in the pattern might match to a "real"
generator (not a boundary) in the context.

Boundary ports (of the pattern) are allowed to match *any port* in the graph,
so they should be matched last - after their "other half".

If both are boundaries, then matching is trivial - just pick any unmatched
port.

# Matching

Matching works by trying to exploit the "determined structure" of the pattern.
For example, suppose we have a graph like this:

    ---\     /---          +---\     /---o
        +---o       in          +---o
    ---/     \---          +---/     \---o

If we have matched the two "center wires", then the rest of the match is deterministic - there is no choice to make.

To exploit it, we just choose the order in which we try to match nodes.
Specifically, we choose an ordering in which all but the first node will
already have one of their hyperedges in the matching, making the other
deterministic.


    p ----- q     // pattern

    |       |
    |       |

    p' ---- q'    // context

(Port Boundary j, Port (Gen 0) i)
(Port (Gen 1) k,  Port (Gen 2) i)
