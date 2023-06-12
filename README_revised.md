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

The proof should take the shape of a series of chained-together equalities of Haskell programs. Each equality should be justified by either the letter of a definition (such as (m) for definition clause 2 of map) or should be marked as (I.H.) if it follows from the use of an induction hypothesis. Every induction hypothesis should be clearly stated
Claim: For any YamlTree t, length . longestPath t = depth t
Proof:
Base case: We distinguish the cases where t is a leaf so  it is of shape : YamlTree [("st", YamlTree [])]

depth $ YamlTree [("st", YamlTree [])]    
= Right Handside
depth $ YamlTree [("st", YamlTree []): []]
=    singleton list
1 + maximum ((depth . snd) ("st", YamlTree [])  : map (depth . snd) []) 
= b
1 + maximum ((depth . snd) ("st", YamlTree []) : [])
= l
1 + maximum (depth YamlTree [])
= n
1 + maximum 0
= a
1  + 0
= j
1

length . longestPath (YamlTree [("st", YamlTree [])])
= Right Handside
length (let (k, v) = maxDepthPair [("st", YamlTree [])] in k : longestPath v)
= d
length (let (k, v) =  ("st", YamlTree []) in k : longestPath v)
= g
length ( "st" : longestPath (YamlTree []))
= substitution
1 + length (longestPath (YamlTree []))
= f
1 + length []
= c
1 + 0
= e 
1 


Inductive case: We assume the property holds for the children trees xs and  prove it for the node YamlTree (x:xs).

Induction Hypotheses:

length . longestPath = depth for xs 
length . longestPath (YamlTree xs) = depth (YamlTree xs) -- (I.H.)
We need to show that length . longestPath = depth for YamlTree (x:xs).

length . longestPath (YamlTree (x:xs))
= Left Hand Side
length (let (k, v) = maxDepthPair (x:xs) in k : longestPath v)
= d
length (let (k, v) = if depth (snd x) >= depth (snd (maxDepthPair xs)) then x else maxDepthPair xs in k : longestPath v)
= h & i
To proceed further, we need to consider these two cases:

Case 1: depth (snd x) >= depth (snd (maxDepthPair xs))
length (let (k, v) = x in k : longestPath v)
= h
length (fst x : longestPath (snd x))
= d
1 + length (longestPath (snd x))
= f
1 + depth (snd x)
= I.H. 
1 + depth (("st", YamlTree zs))
= data definition of YamlTree
1 + maximum ((depth . snd)("st", YamlTree zs)  : map (depth . snd) []) 
= b
1 + maximum ((depth . snd)("st", YamlTree zs) : [])
= l
1 + maximum (depth (snd ("st", YamlTree zs)))
= p
1 + maximum (depth (YamlTree zs))
= n
depth (YamlTree zs)
= b 


Case 2: depth (snd x) < depth (snd (maxDepthPair xs))
maxDepthPair xs
= i
length (let (k, v) = maxDepthPair xs in k : longestPath v)
= d
length (longestPath (maxDepthPair xs))
= d
length (longestPath ("st", YamlTree zs))
= substituting an example of a pair with max depth
length (("st": longestPath (YamlTree zs))
= d
1 + length (longestPath (YamlTree zs))
= f
depth (YamlTree zs)
= I.H.
Therefore, by applying equational reasoning and induction, we have shown that for any YamlTree t, the equality length . longestPath t = depth t holds. This demonstrates the equivalence between the length of the longest path in a YamlTree and its depth.