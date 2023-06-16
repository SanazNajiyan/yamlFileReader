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
We distinguish the cases where t = Leaf and t = YamlTree (x:xs)

base case: Leaf =  YamlTree [("st", YamlTree [])]

length . longestPath (YamlTree [("st", YamlTree [])])
= p
length (longestPath ( YamlTree [("st", YamlTree [])] ))
= d
length (let (k, v) = maxDepthPair [("st", YamlTree [])] in k : longestPath v)
= g
length (let (k, v) =  ("st", YamlTree []) in k : longestPath v)
= q
length ("st" : longestPath YamlTree [])
= c
length ("st" : [])
= f
1 + length []
= e
1 + 0 = 1
= j
1 + maximum (0:[])
= a
1 + maximum ((depth YamlTree []): [])
= n
1 + maximum ((depth . snd) ("st", YamlTree []) : [])
= l
1 + maximum ((depth . snd) ("st", YamlTree [])  : map (depth . snd) []) 
= b
depth $ YamlTree [("st", YamlTree [])]    


Induction step: t = YamlTree (x:xs)
We assume the IH: t' = YamlTree ys
length . longestPath (YamlTree ys) = depth (YamlTree ys) -- (I.H.)
this is of shape:
length . longestPath YamlTree [("st", YamlTree ys')] 
= depth (YamlTree [("st", YamlTree ys')]) 

To prove our inductive case, we split our proof in to distinctive but complement cases.

Case 1: Assumption - depth (snd x) >= depth (snd (maxDepthPair xs))

depth (YamlTree (x:xs))
= b 
1 + maximum ((depth . snd) x : map (depth . snd) xs)
= p
1 + maximum (depth (snd x) : map (depth (snd xs)))
= k
1+ max (depth (snd x)) (maximum (map (depth (snd xs))))
= i2
1 + max (depth (snd x)) (depth (snd (maxDepthPair xs)))
= based on Case 1
1 + depth (snd x)
= I.H.
1 + length (longestPath (snd x))
= f
length (fst x : longestPath (snd x))
= q
length (let (k, v) = x in k : longestPath v)
= h
length (let (k, v) = maxDepthPair (x:xs) in k : longestPath v )
= d
length (longestPath (YamlTree (x:xs)))
= p
(length . longestPath) (YamlTree (x:xs))

Case 2: Assumption - depth (snd x) < depth (snd (maxDepthPair xs))

depth (YamlTree (x:xs))
= b 
1 + maximum ((depth . snd) x : map (depth . snd) xs)
= p
1 + maximum (depth (snd x) : map (depth (snd xs)))
= k
1+ max (depth (snd x)) (maximum (map (depth (snd xs))))
= i2
1 + max (depth (snd x)) (depth (snd (maxDepthPair xs)))
= based on Case 2
1 + depth (snd (maxDepthPair xs))
= I.H.
1 + length (longestPath (snd (maxDepthPair xs)))
= f
length (fst (maxDepthPair xs) : longestPath (snd (maxDepthPair xs)))
= q
length (let (k, v) = maxDepthPair xs in k : longestPath v)
= i
length (let (k, v) = maxDepthPair (x:xs) in k : longestPath v )
= d
length (longestPath (YamlTree (x:xs)))
= p
(length . longestPath) (YamlTree (x:xs))
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
