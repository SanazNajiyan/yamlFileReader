You can assume the following definitions.

data YamlTree = YamlTree [(String, YamlTree)]
example :: YamlTree
example = YamlTree [("Animal", YamlTree [("Dog", YamlTree []),

                                         ("Cat", YamlTree [])]),

                    ("Fruit", YamlTree [("Apple", YamlTree [("Pink Lady", YamlTree [])]),

                                        ("Pear", YamlTree [])])]




depth :: YamlTree -> Int
depth (YamlTree [])     = 0 -- (a)
depth (YamlTree (x:xs)) = 1 + maximum ((depth . snd) x : map (depth . snd) xs) -- (b)


longestPath :: YamlTree -> [String]
longestPath (YamlTree []) = [] -- (c)
longestPath (YamlTree xs) = let (k, v) = maxDepthPair xs in k : longestPath v -- (d)

length :: [a] -> Int
length []       = 0 -- (e)
length (_ : xs) = 1 + length xs -- (f)

maxDepthPair :: [(String, YamlTree)] -> (String, YamlTree)
maxDepthPair [x] = x -- (g)
maxDepthPair (x:xs)
    | depth (snd x) >= depth (snd (maxDepthPair xs)) = x -- (h)
    | otherwise = maxDepthPair xs -- (i)

maximum :: [Int] -> Int
maximum [x]      = x -- (j)
maximum (x : xs) = max x (maximum xs) -- (k)

map :: (a -> b) -> [a] -> [b]
map _ []       = [] -- (l)
map f (x : xs) = f x : map f xs -- (m)

snd :: (a, b) -> b
snd (_, y) = y -- (n)

max :: Int -> Int -> Int
max x y = if x > y then x else y -- (o)

(.) :: (b -> c) -> (a -> b) -> (a -> c)
(.) f g x = f (g x) -- (p)

Please give a formal equational reasoning proof showing that 
length . longestPath = depth

The proof should take the shape of a series of chained-together equalities of Haskell programs. Each equality should be justified by either the letter of a definition (such as (m) for definition clause 2 of map) or should be marked as (I.H.) if it follows from the use of an induction hypothesis. Every induction hypothesis should be clearly stated.

You can assume the following definitions.

data YamlTree = YamlTree [(String, YamlTree)]
example :: YamlTree
example = YamlTree [("Animal", YamlTree [("Dog", YamlTree []),

                                         ("Cat", YamlTree [])]),

                    ("Fruit", YamlTree [("Apple", YamlTree [("Pink Lady", YamlTree [])]),

                                        ("Pear", YamlTree [])])]




depth :: YamlTree -> Int
depth (YamlTree [])     = 0 -- (a)
depth (YamlTree (x:xs)) = 1 + maximum ((depth . snd) x : map (depth . snd) xs) -- (b)


longestPath :: YamlTree -> [String]
longestPath (YamlTree []) = [] -- (c)
longestPath (YamlTree xs) = let (k, v) = maxDepthPair xs in k : longestPath v -- (d)

length :: [a] -> Int
length []       = 0 -- (e)
length (_ : xs) = 1 + length xs -- (f)

maxDepthPair :: [(String, YamlTree)] -> (String, YamlTree)
maxDepthPair [x] = x -- (g)
maxDepthPair (x:xs)
    | depth (snd x) >= depth (snd (maxDepthPair xs)) = x -- (h)
    | otherwise = maxDepthPair xs -- (i)

depth (snd (maxDepthPair xs)) = maximum (map (depth.snd) xs) -- (i2)

maximum :: [Int] -> Int
maximum [x]      = x -- (j)
maximum (x : xs) = max x (maximum xs) -- (k)

map :: (a -> b) -> [a] -> [b]
map _ []       = [] -- (l)
map f (x : xs) = f x : map f xs -- (m)

snd :: (a, b) -> b
snd (_, y) = y -- (n)

max :: Int -> Int -> Int
max x y = if x > y then x else y -- (o)

(.) :: (b -> c) -> (a -> b) -> (a -> c)
(.) f g x = f (g x) -- (p)

(a,b) == (c,d) => a==c && b==d -- (q)

Please give a formal equational reasoning proof showing that 
length . longestPath = depth

The proof should take the shape of a series of chained-together equalities of Haskell programs. Each equality should be justified by either the letter of a definition (such as (m) for definition clause 2 of map) or should be marked as (I.H.) if it follows from the use of an induction hypothesis. Every induction hypothesis should be clearly stated

Claim: for all YamlTrees t, (length . longestPath) t = depth t
Proof:
BASE CASE: the smallest YamlTree is an Empty YamlTree which is YamlTree []
depth (YamlTree []) = (length . longestPath) (YamlTree [])
depth (YamlTree [])  
= a
0
=
0
= e
0
length []
= c
length (longestPath (YamlTree []))
= p
(length . longestPath) (YamlTree [])
INDUCTIVE CASE: 
YamlTree is not a binary tree. It can have n children therefore we assume that for each child the Induction Hypothesis holds.
suppose xs = [(n_1, t_1), (n_2, t_2), ..., (n_n, t_n)]
Therefore, we assume n Induction Hypothesis:
IH_1: (length . longestPath) t_1 = depth t_1
IH_2: (length . longestPath) t_2 = depth t_2
...
IH_n: (length . longestPath) t_n = depth t_n

We now need to prove this: 
(length . longestPath) (YamlTree xs) = depth (YamlTree xs)
Proof: 
depth (YamlTree xs)
= Inductive Case
depth (YamlTree [(n_1, t_1), (n_2, t_2), ..., (n_n, t_n)])
= lists
depth (YamlTree ((n_1, t_1) : nts))
= b
1 + maximum ((depth . snd) (n_1, t_1) : map (depth . snd) nts)
= p
1 + maximum (depth (snd (n_1, t_1)) : map (depth . snd) nts)
= n
1 + maximum ((depth t_1) : map (depth . snd) nts)
= k
1 + max (depth t_1) (maximum (map (depth . snd) nts))
= i2
1 + max (depth t_1) (depth (snd (maxDepthPair nts)))
= here we need to assume two case here to use Lemma o. 
Case 1 : We assume that depth t_1 > depth (snd (maxDepthPair nts))
= o
1 + depth t_1
= IH_1
1 + (length . longestPath) t_1

Case 2: We assume that depth t_1 < depth (snd (maxDepthPair nts))
= o
1 + maximum (map (depth . snd) nts)
= i2
1 + depth (snd (maxDepthPair nts))
= list
1 + depth (snd (maxDepthPair ((n_2, t_2) : nts))) *
= we need to assume two cases to use lemmas h or i

Case 2A: We assume that depth (snd (n_2, t_2)) >= depth (snd (maxDepthPair nts))
= h
1 + depth (snd (n_2, t_2)) 
= n
1 + depth t_2
= IH_2
1 + (length . longestPath) t_2 

Case 2B: We assume that depth (snd (n_2, t_2)) < depth (snd (maxDepthPair nts))
= i
1 + depth (maxDepthPair nts)
= list format
1 + depth (maxDepthPair ((n_3, t_3): nts))
= referring to Case 2A and repeating the same pattern we can conclude that 
1+ depth (snd (n_3, t_3))
= n
1 + depth t_2
= IH_3
1 + (length . longestPath) t_2
Or referring to Case 2B again and extracting the list until we find the maxDepthPair in the first tuple and use the IH_i to prove the equation.






------------------------------------------------------------------------------------
Proof of i2 with induction:
Claim: depth (snd (maxDepthPair xs)) = maximum (map (depth.snd) xs)

base case:
depth (snd (maxDepthPair [x]))
= g
depth (snd x)

depth (snd x)
= j
maximum ((depth.snd) x : [])
= l
maximum ((depth.snd) x : map (depth.snd) [])
= m
maximum (map (depth.snd) [x])

Induction hypothesis:
depth (snd (maxDepthPair ys)) = maximum (map (depth.snd) ys) -- (I.H.)

Inductive case:
depth (snd (maxDepthPair (z:zs))) = maximum (map (depth.snd) (z:zs))

case 1: depth (snd z) >= depth (snd (maxDepthPair zs))
depth (snd (maxDepthPair (z:zs)))
= h
depth (snd z)
= based on case 1
max ((depth.snd) z) (depth (snd (maxDepthPair zs)))
= I.H.
max ((depth.snd) z) (maximum (map (depth.snd) zs))
= k
maximum((depth.snd) z : map (depth.snd) zs)
= m
maximum (map (depth.snd) (z:zs))

case 2: depth (snd z) < depth (snd (maxDepthPair zs))
depth (snd (maxDepthPair (z:zs)))
= i
depth (snd (maxDepthPair zs))
= based on case 2
max ((depth.snd) z) (depth (snd (maxDepthPair zs)))
= I.H.
max ((depth.snd) z) (maximum (map (depth.snd) zs))
= k
maximum((depth.snd) z : map (depth.snd) zs)
= m
maximum (map (depth.snd) (z:zs))
