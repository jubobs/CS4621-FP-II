-- CS4621 Autumn 2013 exam

-- Question 1
-- a)
data BST a = EmptyBST | Node (BST a) a (BST a)

-- b)
-- checkBST bst : is BST 'bst' actually a valid binary search tree?
checkBST :: Ord a => BST a -> Bool
checkBST = isStrictlyIncreasing . inOrderTraversal

-- inOrderTraversal bst : the list of nodes in BST 'bst' collected "in order"
inOrderTraversal :: BST a -> [a]
inOrderTraversal bst = go bst []
  where
    go EmptyBST           xs = xs
    go (Node lsub v rsub) xs = go lsub (v : go rsub xs)

-- isStrictlyIncreasing xs : is list 'xs' strictly increasing?
isStrictlyIncreasing :: Ord a => [a] -> Bool
isStrictlyIncreasing xs = and $ zipWith (<) xs (tail xs)

-- c)
-- flipBST bst : the binary tree obtained by interchanging all left and right
--               subtrees in BST 'bst'
flipBST :: BST a -> BST a
flipBST EmptyBST = EmptyBST
flipBST (Node lsub v rsub) = Node (flipBST rsub) v (flipBST lsub)


-- Question 2
-- a)
-- Implementation using a data declaration
data Stack a = EmptyStack | Push a (Stack a)

emptyStack = EmptyStack

isEmptyStack EmptyStack = True
isEmptyStack _          = False

push = Push

pop (Push _ s) = s

top (Push x _) = x

-- Implementation using a newtype wrapper for lists
newtype Stack a = Stack { getList :: [a] }

emptyStack = Stack []

isEmptyStack = null . getList

push x s = Stack (x : getList s)

pop s = Stack (tail $ getList s)

top = head . getList

-- b)

module Multiset (Multiset, emptyMultiset, isEmptyMultiset,
                 insert,   delete,        occurs) where

-- I N T E R F A C E   :   P U B L I C ---------------------------------------

-- MultiSet a : a multiset of items of type 'a'

-- emptyMultiset : the empty multiset
emptyMultiset :: Multiset a

-- isEmptyMultiset mset : is multiset 'mset' empty?
isEmptyMultiset :: Multiset a -> Bool

-- occurs x mset : how many times does item 'x' occur in multiset 'mset'?
occurs :: Eq a => a -> Multiset a -> Int

-- insert x mset : the multiset obtained by inserting item 'x'
--                 into multiset 'mset'
insert :: Eq a => a -> Multiset a -> Multiset a

-- delete x mset : the multiset obtained by deleting item 'x'
--                 from multiset 'mset'
delete :: Eq a => a -> Multiset a -> Multiset a

-- I M P L E M E N T A T I O N   :   P R I V A T E ---------------------------
--
type Multiset a = [(a, Int)]

emptyMultiset = []

isEmptyMultiset = null

occurs _ []     = 0
occurs x ((x', n) : mset)
    | x == x'   = n
    | otherwise = occurs x mset

insert x []               = [(x, 1)]
insert x ((x', n) : mset) =
    if x == x' then
        (x', n + 1) : mset
    else
        (x', n) : insert x mset

delete x []               = []
delete x ((x', n) : mset) =
    if x == x' then
        if n == 1 then
            mset
        else
            (x', n - 1) : mset
    else
        (x', n) : delete x mset


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
