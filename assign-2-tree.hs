-- data types are defined by constructor patterns
-- Eq equality can be derived from structural equality
     
-- a binary tree consisting of leaves L or internal nodes N
data BinTree = L | N1 BinTree | N BinTree BinTree deriving (Eq, Show)
--data BinTree = L | N1 [BinTree] deriving (Eq, Show) OLD CODE
     
-- this function creates the full binary tree of size 2^(n+1) -1
makeBinTree 0 = L
--makeBinTree n = N1 (makeBinTree (n-1)) (makeBinTree (n-1))

-- this function creates a binary tree of size n
makeABinTree :: Integer -> BinTree
makeABinTree 1 = L
makeABinTree 2 = N1 (makeABinTree 1)
makeABinTree n = N (makeABinTree (ceiling (fromInteger (n-1)/2))) (makeABinTree (floor (fromInteger (n-1)/2)))
     
-- this function computes the size of a binary tree
size L = 1
size (N t1 t2) = 1 + (size t1) + (size t2)
size (N1 t1) = 1 + (size t1)
-- size (N1 n) = 1 + sum (map size n) OLD CODE