-- CS4621 Summer 2013 exam

-- Question 1
-- a)
module List (List, Nil, Cons, null, head, tail) where

-- I N T E R F A C E : P U B L I C -------------------------------------------

-- List a : an ordered collection of items of type a

-- null xs : is List 'xs' the empty List?
null :: List a -> Bool

-- head xs : the first element of List 'xs'
head :: List a -> a

-- tail xs : the List obtained by removing the first element in List 'xs'
tail :: List a -> List a

-- I M P L E M E N T A T I O N : P R I V A T E -------------------------------

data List a = Nil | Cons a (List a)

null Nil        = True
null (Cons _ _) = False

head (Cons x _) = x

tail (Cons _ xs) = xs

-- b)
-- count x xs : the number that the item 'x' occurs in the List 'xs'
count :: a -> List a -> Int
count x xs = go xs 0
  where
    go Nil n         = n
    go (Cons x' xs') = go xs' (n + (if x == x' then 1 else 0))

-- append xs ys : the List formed from the items in the List 'xs',
--                followed by the items in the List 'ys'
append :: List a -> List a -> List a
append Nil         ys = ys
append (Cons x xs) ys = Cons x (append xs ys)


-- Question 2
--
-- reach g s search : a list of the nodes in graph 'g' reachable from node 's'
--                    collected using the graph-traversal algorithm 'search'
reach :: Graph -> Node -> Search -> [Node]
reach g s search = reach' g [s] [] search

-- reach' g next seen searhc : a list of the nodes in graph 'g' reachable from                                nodes in the list 'next', but excluding nodes
--                             in the list 'seen', collected using the graph-
--                             traversal algorithm 'search'
reach' :: Graph -> [Node] -> [Node] -> Search -> [Node]
reach' _ []       _    _      = []
reach' g (n : ns) seen search
    | n `elem` seen = reach' g ns seen search
    | otherwise     = n : reach' g (join ns (neighbours n g) search)
                                   (n : seen)
                                   search


-- Question 3
-- a)
-- The Factor word 'z', as defined,
--    expects a positive integer ('n') on top of the stack, followed by
--      'n' integers;
--    pops all 'n + 1' integers;
--    pushes the sum of the 'n' integers (following the topmost one) onto
--      the stack.
--
-- For instance,
--
--     1 2 3 4 5 6 7 3 z .
--
-- prints '18' (the value of 5 + 6 + 7), and the resulting stack is
--
--     (bottom)   1 | 2 | 3 | 4   (top)
--
-- b)
-- To implemented 'repeat', add the following case in the definition of
-- 'eval'' (underneath that of 'eval'if', for instance):
if t == "repeat" then
    eval'repeat ts stack env
else
-- ... and define eval'repeat as follows:
eval'repeat :: [Token] -> Stack -> Environment -> String
eval'repeat ts (Sinteger 0 : _            : ss) env =
    eval' ts ss env
eval'repeat ts (Sinteger n : Squotation q : ss) env =
    eval'repeat (q ++ ts) (Sinteger (n - 1) : Squotation q : ss) env
