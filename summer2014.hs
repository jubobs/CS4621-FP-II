-- CS4621 Summer 2014 exam

-- Question 1
-- a)
-- ancestors n bst : the ancestors of node 'n' in BST 'bst'
--                   (under the assumption that 'n' actually occurs in 'bst')
ancestors :: Ord a => a -> BST a -> [a]
ancestors n (Node lsub v rsub) =
    case n `compare` v of
        EQ -> [v]
        LT -> v : ancestors n lsub
        GT -> v : ancestors n rsub

-- b)
-- cca n1 n2 bst : the closest common ancestor of nodes 'n1' and 'n2'
--                 in BST 'bst' (under the assumption that both 'n1' and 'n2'
--                 occur in 'bst')
cca :: Ord a => a -> a -> BST a -> a
cca n1 n2 (Node lsub v rsub)
    | n1 < v && n2 < v = cca n1 n2 lsub
    | n1 > v && n2 > v = cca n1 n2 rsub
    | otherwise        = v


-- Question 2
-- a)
data Stack a = EmptyStack | Push a (Stack a)

emptyStack = EmptyStack

isEmptyStack EmptyStack = True
isEmptyStack _          = False

push = Push

pop (Push _ s) = s

top (Push x _) = x

size EmptyStack = 0
size (Push _ s) = 1 + size s

-- b)
data Stack' a = EmptyStack | Push a (Stack' a)

type Stack a = (Stack' a, Int)

emptyStack = (EmptyStack, 0)

isEmptyStack (_, n) = n == 0

push x (s, n) = (Push x s, n + 1)

pop (Push _ s, n) = (s, n - 1)

top (Push x _, _) = x

size = snd


-- Question 3
-- a)
getDef t ((name, def) : es)
    | t == name = def
    | otherwise = getDef t es

-- b)
eval'if :: [Token] -> Stack -> Environment -> String
eval'if ts (Squotation qT : _             : Sboolean True  : ss) env =
    eval' (qT ++ ts) ss env
eval'if ts (_             : Squotation qF : Sboolean False : ss) env =
    eval' (qF ++ ts) ss env

-- c)
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
