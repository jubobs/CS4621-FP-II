-- CS4621 Autumn 2012 exam

-- Question 1
-- a)
data BST a = EmptyBST | Node (BST a) a (BST a)

-- b)
-- treeSort xs : the list obtained by sorting list 'xs' in ascending order
treeSort :: Ord a => [a] -> [a]
treeSort = inOrderTraversal . fromList

-- fromList xs : the BST obtained by inserting all items from list 'xs' into
--               an empty BST
fromList :: Ord a => [a] -> BST a
fromList = foldr insert EmptyBST

-- insert x bst : the BST obtained by inserting item 'x' in BST 'bst'
insert :: Ord a => a -> BST a -> BST a
insert x EmptyBST = Node EmptyBST x EmptyBST
insert x (Node lsub v rsub)
    | x < v     = Node (insert x lsub) v rsub
    | otherwise = Node lsub v (insert x rsub)

-- inOrderTraversal bst : the list of nodes in BST 'bst' collected "in order"
inOrderTraversal :: BST a -> [a]
inOrderTraversal bst = go bst []
  where
    go EmptyBST           xs = xs
    go (Node lsub v rsub) xs = go lsub (v : go rsub xs)


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
-- A topological sort of a directed acyclic graph is a list of its nodes in
-- which the start node of each edge occurs before the edge's end node.
-- In general, multiple topological sorts are possible (non-uniqueness).

-- c)
-- topSort g : a list of the nodes in the directed acyclic graph 'g',
--             sorted in topological order
topSort :: Graph -> [Node]
topSort g
    | null (nodes g) = []
    | otherwise      = srcs ++ topSort (deleteNodes srcs g)
  where
    srcs = sources g

-- deleteNodes ns g : the graph obtained by deleting all nodes in list 'ns'
--                    from graph 'g'
deleteNodes :: [Node] -> Graph -> Graph
deleteNodes ns g = foldr deleteNode g ns

-- sources g : the lists of the nodes in graph 'g' with no incoming edges

sources :: Graph -> [Node]
sources g = [ n | n <- nodes g, null [ e | e <- edges g, end e == n ]]


-- Question 3
-- a)
-- The definition of a word ends up being added to the environment,
-- regardless of whether that word already has a definition in the
-- environment (see the case 't == ":"' in 'eval'').
--
-- Upon encountering a word that isn't built in, 'getDef' performs a linear
-- search from left to right through the environment, retrieving the
-- definition of the first match found (i.e. the most recent definition of
-- the word in question).
--
-- Therefore, when a word is defined multiple times, the active one is always
-- the most recent one. For example,
--
--     : increase 1 + ;
--     : increase 2 + ;
--
-- results in the environment
--
--     [("increase", ["2", "+"]), ("increase", ["1", "+"])]
--
-- but, in
--
--     3 increase .
--
-- the most recent definition of 'increase' is used, resulting in '5'.
--
-- b)
-- To implement 'call' as a built-in word:
--   Add the following before the case expression in the definition of 'eval'':
if t == "call" then
    eval'call ts stack env
else
--  ... and define 'eval'call' as follows:
eval'call :: [Token] -> Stack -> Environment -> String
eval'call ts (Squotation q : ss) env = eval' (q ++ ts) ss env

-- Incidentally, 'call' can be implemented as a Factor library word;
-- no need to modify the interpreter!
--
--    : call t 2 sink [ ] if ;
--
