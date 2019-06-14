{-

-- ADDING AN EDGE:
-- twist, but with an extra generator that isn't connected up yet.
--disconnectedHyperEdge = twist
--  { signatures  = Map.insert 0 (error "TODO: disconnectedHyperEdge") (signatures twist)
--  , connections = Map.fromList
--  }

-- Finding patterns!
-- Suppose we have an identity-analogue HG.
-- NOTE: TODO: decide if interpretation of "Port" depends on 'side' - affects use of Bimap!!
--             Can't use bimap if depends on position - below example is not
--             representable with bimap.
--
-- edges: Port OpenLeft 0    <->  Port (Gen 0) 0
--        Port (Gen 0) 0  <->  Port OpenRight 0
--
-- Connectivity needs to know that we can get from `Port (Gen 0) 0` to any
-- `Port (Gen 0) i` for all i <- 0..numOutputs gen
-- Requires two lookups in a map, not a huge deal!

-------------------------------
-- "REAL" OPERATIONS

-- Composition!
-- Fiddlier- essentially want to identify ports of the inner boundaries
-- e.g., if composing A and B, and we have in A: (p, Port OpenRight i) and in B: (Port OpenLeft j, q),
-- then we want to replace both with one edge, (p, q).
-- Can make this work for "affine" too, if we join the leftover nodes to the other boundary.

-- Tensor Product!
-- Map edges in B, (Port OpenLeft i, q) -> (Port OpenLeft i+n, q)
-- and also for right boundary.


-}
