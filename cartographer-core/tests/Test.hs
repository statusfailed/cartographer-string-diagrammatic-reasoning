import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Data.Hypergraph.Test (tests)

main = defaultMain tests
