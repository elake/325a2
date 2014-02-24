-- CMPUT 325 W14 Assignment 2
-- Eldon Lake
     
-- a binary tree consisting of leaves L or internal nodes N or N1
data BinTree = L | N1 BinTree | N BinTree BinTree deriving (Eq, Show)
     
-- this function creates the full binary tree of size 2^(n+1) -1
makeBinTree 0 = L
makeBinTree n = N (makeBinTree (n-1)) (makeBinTree (n-1))

-- this function creates a binary tree of size n
makeABinTree :: Integer -> BinTree
makeABinTree 1 = L
makeABinTree 2 = N1 (makeABinTree 1)
makeABinTree n = N (makeABinTree (ceiling (fromInteger (n-1)/2))) (makeABinTree (floor (fromInteger (n-1)/2)))
     
-- this function computes the size of a binary tree
size L = 1
size (N t1 t2) = 1 + (size t1) + (size t2)
size (N1 t1) = 1 + (size t1)

-- this function computes the depth of a binary tree
depth L = 1
depth (N1 t1) = 1 + (depth t1)
depth (N t1 t2) = 1 + max (depth t1) (depth t2)