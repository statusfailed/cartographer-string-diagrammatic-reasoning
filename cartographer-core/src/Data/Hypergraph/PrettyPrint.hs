-------------------------------
-- Pretty printing
-- example:
--
--  signature
--  ---------------
--  0 == <show sig>
--  1 == <show sig>
--
--  wires
--  ---------------
--  0 port 0  --> 1 port 2

prettyPrint :: Show sig => OpenHypergraph sig -> String
prettyPrint (Hypergraph conns signatures _)
  =  "signatures\n"
  ++ "-------------------------------\n"
  ++  prettySignature (Map.toList signatures)
  ++ "\n"
  ++ "wires\n"
  ++ "-------------------------------\n"
  ++ prettyWires (Bimap.toList conns)
  ++ "\n"

prettySignature :: Show a => [(HyperEdgeId, a)] -> String
prettySignature xs =
  xs >>= (\(HyperEdgeId i, sig) -> show i ++ " == " ++ show sig ++ "\n")

prettyWires :: [(Port Source Open, Port Target Open)] -> String
prettyWires xs =
  xs >>= (\(s,t) -> prettyPort s ++ "  -->  " ++ prettyPort t ++ "\n")

prettyPort (Port (Gen (HyperEdgeId e)) i) = show e ++ " port " ++ show i
prettyPort (Port Boundary i) = "B port " ++ show i
