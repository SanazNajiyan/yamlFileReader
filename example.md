-- Claim: combine (noWrite a) f = f a

-- Proof
{-
combine (noWrite a) f
= -- b
combine ("", a) f 
= -- a
("" ++ fst (f a), snd (f a))
= -- c
(fst (f a), snd (f a))
= -- i
f a
-}


-- b. 


-- Some definitions that are specific to this question:

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

flatten :: Tree a -> [a]
flatten (Leaf n) = [n] 
flatten (Node l r) = flatten l ++ flatten r 
 
flattenAcc :: Tree a -> [a] -> [a]
flattenAcc (Leaf n) ns = n : ns 
flattenAcc (Node l r) ns = flattenAcc l (flattenAcc r ns) 


-- Claim: flatten t ++ ns = flattenAcc t ns

-- We prove this by induction on t, distinguishing the cases
-- t = Leaf i and t = Node l r

-- Base case: t = Leaf i 
{-
flatten (Leaf i) ++ ns
= -- e
[i] ++ ns
= -- desugaring list syntax
(i : []) ++ ns
= -- d
i : ([] ++ ns)
= -- c
i : ns
= -- g
flattenAcc (Leaf i) ns

Inductive case: t = Node l r 
we assume the induction hypothesis that 
flatten l ++ ns = flattenAcc l ns
AND 
flatten r ++ ns = flattenAcc r ns
FOR ALL lists ns

then

flatten (Node l r) ++ ns
= f
(flatten l ++ flatten r ) ++ ns
= l
flatten l ++ (flatten r ++ ns)
= I.H.
flatten l ++ (flattenAcc r ns)
= I.H.
flattenAcc l (flattenAcc r ns)
= h
flattenAcc (Node l r) ns

Our claim follows by induction on t.

-}


size :: Tree a -> Int
size Leaf = 0 -- a
size (Node l v r) = size l + (1 + size r) -- b


combineTree :: Tree a -> Tree a -> Tree a
combineTree Leaf r' = r' -- c
combineTree (Node l v r) r' = Node l v (combineTree r r') -- d

-- 0 + i = i -- law e
-- (i + j) + k = i + (j + k) -- law f


{-
Claim: for all Trees l' and r',   size (combineTree l' r') = size l' + size r'



Proof:
We distinguish the cases where l' = Leaf and l' = Node l v r.


Case l' = Leaf
size (combineTree Leaf r')
= c
size r'
= e
0 + size r'
= a
size Leaf + size r'


Case l' = Node l v r
We assume the induction hypotheses
size (combineTree l r') = size l + size r'    (I.H.1, which we won't need - in fact, we could leave this out, but I suspect a lot of students will write it down)
size (combineTree r r') = size r + size r'    (I.H.2, which we will need)


size (combineTree (Node l v r) r')
= d
size (Node l v (combineTree r r'))
= b
size l + (1 + size (combineTree r r'))
= I.H.2
size l + (1 + (size r + size r'))
= f
size l + ((1 + size r) + size r')
= f
(size l + (1 + size r)) + size r'
= b
size (Node l v r) + size r'

The claim now follows by indcution on l'.


-}


We prove the equation using structural induction on xs.

-- base case: xs = [] -- 3 in total

--   map f (foldr (\x r -> g x : r) [] [])
-- = { def foldr -- a } -- 1
--   map f []
-- = { def map -- c } -- 1
--   []
-- = { def map -- c } -- 1
--   map (f . g) []

-- induction step: xs = (x:xs') -- 9 in total
-- We assume the IH: map f (foldr (\x r -> g x : r) [] xs') = map (f . g) xs'. -- 2 

--   map f (foldr (\x r -> g x : r) [] (x:xs'))
-- = { def foldr -- b } -- 1
--   map f ((\x r -> g x : r) x (foldr (\x r -> g x : r) [] xs'))
-- = { function application  } -- 1
--   map f (g x : foldr (\x r -> g x : r) [] xs')
-- = { def map -- d } -- 1
--   f (g x) : map f (foldr (\x r -> g x : r) [] xs')
-- = { def . -- i } -- 1
--   (f . g) x : map f (foldr (\x r -> g x : r) [] xs')
-- = { IH } -- 2
--   (f . g) x : map (f . g) xs'
-- = { def map -- d } -- 1
--   map (f . g) (x:xs')


-- (b)
-- We prove the equation by using extensional reasoning.
-- That is, for all trees t we show that
-- size t = (length . toList) t
-- This, we show by structural induction on t.

-- base case: t = Leaf -- 2 in total

--   size Leaf
-- = { def size -- e } -- 1/2
--   0
-- = { def length } -- 1/2
--   length []
-- = { def toList -- g } -- 1/2
--   length (toList Leaf)
-- = { def . -- i } -- 1/2
--   (length . toList) Leaf


-- induction step: t = Node l x r -- 7 in total
-- We assume the IH: size t' = (length . toList) t', for t' in {l,r} -- 2

--   size (Node l x r)
-- = { def size -- f } 1/2    --- round up in case of half points
--   size l + 1 + size r
-- = { IH 2x } -- 2/2
--   (length . toList) l + 1 + (length . toList) r
-- = { def . 2x -- i } 1/2
--   length (toList l) + 1 + length (toList r)
-- = { def length } 1/2
--   length (toList l) + length [x] + length (toList r)
-- = { lemma -- j } 1/2
--   length (toList l ++ [x] ++ toList r)
-- = { def toList -- h } 1/2
--    length (toList (Node l x r))
-- = { def . -- i } 1/2
--    (length . toList) (Node l x r)
