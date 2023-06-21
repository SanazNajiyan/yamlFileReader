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

Assuming that IH_1 ... IH_n for (x:xs), then
depth (snd (maxDepthPair (x:xs))) 
= (length . longestPath) (snd (maxDepthPair (x:xs))) -- (r)  

Please give a formal equational reasoning proof showing that 
length . longestPath = depth

The proof should take the shape of a series of chained-together equalities of Haskell programs. Each equality should be justified by either the letter of a definition (such as (m) for definition clause 2 of map) or should be marked as (I.H.) if it follows from the use of an induction hypothesis. Every induction hypothesis should be clearly stated

Claim: for all YamlTrees t, (length . longestPath) t = depth t
Proof:
We will prove this claim using structural induction on the YamlTree t

Base Case: 
Consider the smallest YamlTree, which is an empty YamlTree as follows :  YamlTree []
So we have
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
Thus the claim holds for the Base Case

Inductive Case: 
Assume the claim holds for all YamlTrees with n nodes.
Let xs = [(n_1, t_1), (n_2, t_2), ..., (n_n, t_n)] be a list of pairs representing the children of the YamlTree xs.

Therefore, we assume n Induction Hypotheses. So for each child one, Induction Hypothesis as follows:
IH_1: (length . longestPath) t_1 = depth t_1
IH_2: (length . longestPath) t_2 = depth t_2
...
IH_i: (length . longestPath) t_i = depth t_i
...
IH_n: (length . longestPath) t_n = depth t_n

Assuming that IH_1, IH_2,...IH_n holds 

We now need to prove this: 
(length . longestPath) (YamlTree (x:xs)) = depth (YamlTree (x:xs))

Proof: 
depth (YamlTree (x:xs))
= substituting xs and x = (n,t) according to data definition of YamlTree
depth (YamlTree ((n,t):[(n_1, t_1), (n_2, t_2), ..., (n_n, t_n)]))
= b
1 + maximum ((depth . snd) (n, t) : map (depth . snd) [(n_1, t_1), ..., (n_n, t_n)])
= p
1 + maximum (depth (snd (n, t)) : map (depth (snd [(n_1, t_1), ..., (n_n, t_n)])))
= n
1 + maximum ((depth t) : map (depth [t_1, t_2,..., t_n]))
= simpler
1 + maximum ((depth t) : map depth [t_1, t_2,..., t_n])
= m
1 + maximum ((depth t) : (depth t_1 : map depth [t_2,..., t_n]))
= m 
1 + maximum (depth t : [depth t_1, ..., ((depth t_n): map depth t_n [] )])
= l
1 + maximum (depth t : [depth t_1, ..., depth t_n])
= k
1 + max (depth t) (maximum [depth t_1, ..., depth t_n]) *
= here we need to assume 2 cases to apply o
Case 1 : We assume that depth t > maximum [depth t_1, ..., depth t_n]
= o
1 + depth t
= IH_i
1 + (length . longestPath) t
Therefore, by assuming Case 1 and using the induction hypothesis (IH_i), we have shown that depth (YamlTree (x:xs)) equals (length . longestPath) YamlTree (x:xs)

Case 2: We assume that depth t <= maximum [depth t_1, ..., depth t_n]
= o
1 + maximum [depth t_1, ..., depth t_n]
= n
1 + maximum (map (depth . snd) [(n_1, t_1), ..., (n_n, t_n)])
= i2
1 + depth (snd (maxDepthPair [(n_1, t_1), ..., (n_n, t_n)]))
= list  
1 + depth (snd (maxDepthPair ((n_1, t_1):[(n_2, t_2), ..., (n_n, t_n)])))
= r
1 + (length . longestPath) (snd (maxDepthPair ((n_1, t_1):[(n_2, t_2), ..., (n_n, t_n)])))

= here we need to assume 2 cases in order to use the rules for maxDepthPair

Case 2A: we assume that depth (snd (n_1, t_1)) >= depth (snd (maxDepthPair [(n_2, t_2), ..., (n_n, t_n)]))
= h
1 + depth (snd (n_1, t_1))
= n
1 + depth t_1
= IH_1
1 + (length . longestPath) t_1

Case 2B: we assume that depth (snd (n_1, t_1)) < depth (snd (maxDepthPair [(n_2, t_2), ..., (n_n, t_n)]))
= i
1 + depth (snd (maxDepthPair [(n_2, t_2), ..., (n_n, t_n)]))
= r
1 + (length . longestPath) (snd (maxDepthPair[(n_2, t_2), ..., (n_n, t_n)]))
= this will be followed again like Case 2A and applying IH_2
Therefore we can eventually prove:

(length . longestPath) (YamlTree (x:xs)) = depth (YamlTree (x:xs))

Therefore, we have proven that for all YamlTrees t, (length . longestPath) t = depth t.

-------------------------------------------------------------------------------------------
Proof of Lemma r with induction:
Claim: We assume that IH_1, IH_2...IH_n holds for any subtree in the list xs. 
depth (snd (maxDepthPair (x:xs))) = (length . longestPath) (snd (maxDepthPair (x:xs)))


base case: xs = []
In this case, (x:xs) reduces to [x], where x = (n,t)

depth (snd (maxDepthPair [x]))
= q
depth (snd (maxDepthPair [(n,t)]))
= g 
depth (snd (n,t))
= n
depth t
(length . longestPath) t
= n
(length . longestPath) (snd (n,t))
= g
= (length . longestPath) (snd (maxDepthPair [(n,t)]))
= q
(length . longestPath) (snd (maxDepthPair [x]))
Hence, in the base case, the equation holds.

Inductive Case: Assume the equation holds for xs.

We therefore assume this induction hypothesis:
(length . longestPath) (snd (maxDepthPair xs)) = depth (snd (maxDepthPair xs)) -- (I.H.)

and we need to prove that equation holds for (x:xs)

depth (snd (maxDepthPair (x:xs))) = (length . longestPath) (snd (maxDepthPair (x:xs)))

depth (snd (maxDepthPair ((n,t):xs)))
= We need to assume two cases where:
Case 1 : depth (snd (n,t)) >= depth (snd (maxDepthPair xs)) 
= h
depth (snd (n,t))
= n
depth t
= IH_1, IH_2,.. IH_n
(length . longestPath) t
= n
(length . longestPath) (snd (n,t))
= h
(length . longestPath) (snd (maxDepthPair (x:xs)))

Therefore, in Case 1, we have:

depth (snd (maxDepthPair (x:xs))) = (length . longestPath) (snd (maxDepthPair (x:xs)))

and Case 2: depth (snd (n,t)) < depth (snd (maxDepthPair xs))
= i
depth (snd maxDepthPair xs)
= I.H.
(length . longestPath) (snd (maxDepthPair xs))
= according to the assumption we made for Case 2 xs is indeed the same as (x:xs)

Therefore, in Case 2, we have:

depth (snd (maxDepthPair (x:xs))) = (length . longestPath) (snd (maxDepthPair (x:xs)))

Since we have covered both cases, we can conclude that in both cases, the equation holds:

depth (snd (maxDepthPair (x:xs))) = (length . longestPath) (snd (maxDepthPair (x:xs)))

By proving the base case and the inductive case, we have shown that the equation holds for all YamlTrees. Hence, the claim is proven.




------------------------------------------------------------------------------------
Proof of Lemma i2 with induction:
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
