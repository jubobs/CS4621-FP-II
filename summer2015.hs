-- CS4621 Summer 2015 exam

-- Question 1
-- a)
-- takeBT n bt : the subtree of the BT 'bt' formed by all nodes whose depth
--               is at most the integer 'n'
takeBT :: Node -> BT a -> BT a
takeBT n bt
    | n <  0    = error "takeBT : negative integer argument"
    | n == 0    = EmptyBT
    | otherwise = let n' = n - 1 in
          case bt of
              EmptyBT            -> EmptyBT
              (Node lsub v rsub) -> Node (takeBT n' lsub) v (takeBT n' rsub)

-- b)
-- depthBT : an infinite BT, containing no empty subtrees, where the value
--           stored at each node equals its depth
depthBT :: BT Integer
depthBT = go 1
  where
    go n = let bt in Node bt n bt


-- Question 2
-- A modified version of breadth-first search, in which we annotate nodes
-- to visit with their distance from the origin, does the trick:

-- distances g begin : a list of tuples, consisting of all nodes
--                     in graph 'g' reachable from node 'begin' and
--                     their distances from it
distances :: Graph -> Node -> [(Node, Int)]
distances g begin = go g [(begin, 0)] []
  where
    go _ []            _    = []
    go g ((v, d) : ps) seen
        | v `elem` seen     = go g ps seen
        | otherwise         =
              (v, d) : go g (ps ++ map (flip (,) (d + 1)) (neighbours v g))
                            (v : seen)

-- For more robustness, 'distances' should check that 'begin' is an actual
-- node of graph 'g' before calling the helper, but what the 'Graph' module
-- exports doesn't allow us to do that.


-- Question 3
-- a)
-- The Factor word 'f', as defined,
--    expects a positive integer ('n') on top of the stack, followed by
--      'n' integers;
--    pops all 'n + 1' integers;
--    pushes the sum of the 'n' integers (following the topmost one) onto
--      the stack.

-- For instance,
--
--     1 2 3 4 5 3 f .
--
-- prints '12' (the value of 3 + 4 + 5), and the resulting stack is
--
--     (bottom)   1 | 2   (top)

-- b)
-- To implemented 'times', add the following case in the definition of
-- 'eval'' (underneath that of 'eval'if', for instance):
if t == "times" then
    eval'times ts stack env
else
-- ... and define eval'times as follows:
eval'times :: [Token] -> Stack -> Environment -> String
eval'times ts (Sinteger 0 : _            : ss) env = eval' ts ss env
eval'times ts (Sinteger n : Squotation q : ss) env =
    eval'times (q ++ ts) (Sinteger (n - 1) : Squotation q : ss) env

-- 'times' cannot be implemented as a Factor library word, as far as I know.
