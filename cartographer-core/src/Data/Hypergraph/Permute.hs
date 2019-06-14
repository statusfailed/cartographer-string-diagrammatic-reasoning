

-- | An n → n permutation of wires.
-- NOTE: the function supplied must be a bijection!
permute :: (Int -> Int) -> Int -> OpenHypergraph sig
permute f n = Hypergraph conns Map.empty 0 where
  conns = Bimap.fromList [ (i, f i) | 0..n-1]
