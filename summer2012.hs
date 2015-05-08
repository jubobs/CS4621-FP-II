-- CS4621 Summer 2012 exam

-- Question 1
-- a)
data BST a = EmptyBST | Node (BST a) a (BST a)

-- b)
-- sumBST bst : the sum of all the values stored in BST 'bst'
sumBST :: BST Int -> Int
sumBST EmptyBST = 0
sumBST (Node lsub v rsub) = sumBST lsub + v + sumBST rsub

-- c)
-- sameShape bst1 bst2 : do BSTs 'bst1' and 'bst2' have the same shape?
sameShape :: BST a -> BST b -> Bool
sameShape EmptyBST             EmptyBST             = True
sameShape (Node lsub1 _ rsub1) (Node lsub2 _ rsub2) =
    sameShape lsub1 lsub2 && sameShape rsub1 rsub2
sameShape _                    _                    = False


-- Question 2
-- a)
------------------------------------------------------------------------------

module Graph (Graph, Node, Edge, emptyGraph, nodes, edges,
              node, label, edge, start, end,
              insertNode, deleteNode, insertEdge, deleteEdge) where

------------------------------------------------------------------------------

-- I N T E R F A C E    ( P U B L I C ) -------------------------------------

-- Graph : a directed unweighted graph

-- Node : a node (an instance of 'Eq')

-- Edge : an edge (an instance of 'Eq')

-- emptyGraph : the empty graph
emptyGraph :: Graph

-- nodes g : a list of the nodes in graph 'g'
nodes :: Graph -> [Node]

-- edges g : a list of the edges in graph 'g'
edges :: Graph -> [Edge]

-- node s : the node with label 's'
node :: String -> Node

-- label n : the label of the node 'n'
label :: Node -> String

-- edge n1 n2 : an edge from node 'n1' to code 'n2'
edge :: Node -> Node -> Edge

-- start e : the start node of edge 'e'
start :: Edge -> Node

-- end e : the end node of edge 'e'
end :: Edge -> Node

-- insertNode n g : the graph obtained by inserting node 'n' into graph 'g'
insertNode :: Node -> Graph -> Graph

-- deleteNode n g : the graph obtained by deleting node 'n' from graph 'g'
deleteNode :: Node -> Graph -> Graph

-- insertEdge e g : the graph obtained by inserting edge 'e' into graph 'g'
insertEdge :: Edge -> Graph -> Graph

-- deleteEdge e g : the graph obtained by deleting edge 'e' from graph 'g'
deleteEdge :: Edge -> Graph -> Graph

-- b)
-- dfs g s : a list of the nodes in graph 'g' reachable from node 's'
dfs :: Graph -> Node -> [Node]
dfs g s = dfs' g [s] []

-- dfs' g next seen : a list of the nodes in graph 'g' reachable from nodes
--                    in the list 'next', but excluding nodes in the list
--                    'seen'
dfs' :: Graph -> [Node] -> [Node] -> [Node]
dfs' _ []       _    = []
dfs' g (n : ns) seen
    | n `elem` seen = dfs' g ns seen
    | otherwise     = n : dfs' g (neighbours n g ++ ns) (n : seen)

-- neighbours n g : a list of the nodes reachable along a single out-edge
--                  from node 'n' in graph 'g'
neighbours :: Node -> Graph -> [Node]
neighbours n g = [end e | e <- edges g, start e == n]

-- c)
-- isConnected g : is the undirected graph 'g' connected?
isConnected :: Graph -> Bool
isConnected g = null ns || length (dfs g n) == length ns
  where
    ns = nodes g
    n  = head ns


-- Question 3
-- a)
-- To implement 'and' as a built-in word, augment the case expression in
-- 'eval'' by the following case (before the catch-all pattern):
"and" -> eval' ts (apply'and stack) env
-- and define 'apply'and' as follows:
apply'and :: Stack -> Stack
apply'and (Sboolean b2 : Sboolean b1 : ss) = Sboolean (b1 && b2) : ss

-- b)
-- By the time our 'and' word gets applied, the two Boolean values on the
-- stack have already been evaluated, at least as far as Factor is concerned
-- (On the Haskell side, though, those Booleans may not yet have been
-- completely evaluated, if their value hasn't been needed yet).

-- c)
-- Syntax: 'lazyAnd' expects
--    a quotation as the topmost item on the stack
--      (containing an expression of type Boolean)
--    a Boolean as the second topmost item on the stack,
--
-- To implemented 'lazyAnd' as a built-in, add the following case in the
-- definition of 'eval'' (underneath that of 'eval'if', for instance):
if t == "lazyAnd" then
    eval'lazyAnd ts stack env
else
-- ... and define eval'lazyAnd as follows:
eval'lazyAnd :: [Token] -> Stack -> Environment -> String
eval'lazyAnd ts (_            : Sboolean False : ss) env =
    eval' ts (Sboolean False : ss) env
eval'lazyAnd ts (Squotation q : Sboolean True  : ss) env =
    eval' (q ++ ts) ss env
--
-- Incidentally, 'lazyAnd' can be implemented as a Factor library word;
-- no need to modify the interpreter!
--
--     : lazyAnd [ f ] if ;
--
