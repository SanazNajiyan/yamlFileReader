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
Claim: For any YamlTree t, length . longestPath t = depth t
Proof:
Base case: We distinguish the cases where t is a leaf so  it is of shape : YamlTree [("st", YamlTree [])]

We will start from the Right Handside
depth $ YamlTree [("st", YamlTree [])]    
= b
1 + maximum ((depth . snd) ("st", YamlTree [])  : map (depth . snd) []) 
= l
1 + maximum ((depth . snd) ("st", YamlTree []) : [])
= p
1 + maximum (depth (snd ("st", YamlTree [])) : [])
= n
1 + maximum ((depth YamlTree []): [])
= a
1 + maximum (0:[])
= j
1  + 0 = 1

length . longestPath ( YamlTree [("st", YamlTree [])] )
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

Inductive case: We assume the property holds for the children trees xs and  prove it for the node YamlTree (x:xs).

Induction Hypotheses:

length . longestPath = depth for xs 
length . longestPath (YamlTree xs) = depth (YamlTree xs) -- (I.H.)
Inductive Case: We need to show:
length . longestPath (YamlTree (x:xs)) = depth (YamlTree (x:xs))

To prove our inductive case, we split our proof in to distinctive but complement cases.

Case 1: depth (snd x) >= depth (snd (maxDepthPair xs))

depth (YamlTree (x:xs))
= b 
1 + maximum ((depth . snd) x : map (depth . snd) xs)
= p
1 + maximum (depth (snd x) : map (depth (snd xs)))
= k
1+ max (depth (snd x)) (maximum (map (depth (snd xs))))
= i2
1 + max (depth (snd x)) (depth (snd (maxDepthPair xs)))
= based on case 1

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
length . longestPath (YamlTree (x:xs))


Case 2: depth (snd x) < depth (snd (maxDepthPair xs))

depth (YamlTree (x:xs))
= b 
1 + maximum ((depth . snd) x : map (depth . snd) xs)
= p
1 + maximum (depth (snd x) : map (depth (snd xs)))
= k
1+ max (depth (snd x)) (maximum (map (depth (snd xs))))
= i2
1 + max (depth (snd x)) (depth (snd (maxDepthPair xs)))
= based on case 2

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
length . longestPath (YamlTree (x:xs))


proof of i2 on induction:
depth (snd (maxDepthPair xs)) = maximum (map (depth.snd) xs)

base case:
depth (snd (maxDepthPair [x]))
= g

depth (snd x)

= j
maximum ((depth.snd) x : [])
= l
maximum ((depth.snd) x : map (depth.snd) [])
= m
maximum (map (depth.snd) [x])

Induction hypothesis:
depth (snd (maxDepthPair xs)) = maximum (map (depth.snd) xs)

Inductive case:
depth (snd (maxDepthPair (x:xs))) = maximum (map (depth.snd) (x:xs))

case 1: depth (snd x) >= depth (snd (maxDepthPair xs))
depth (snd (maxDepthPair (x:xs)))
= h

depth (snd x)

= based on case 1
max ((depth.snd) x) (depth (snd (maxDepthPair xs)))
= I.H.
max ((depth.snd) x) (maximum (map (depth.snd) xs))
= k
maximum((depth.snd) x : map (depth.snd) xs)
= m
maximum (map (depth.snd) (x:xs))

case 2: depth (snd x) < depth (snd (maxDepthPair xs))
depth (snd (maxDepthPair (x:xs)))
= i

depth (snd (maxDepthPair xs))

= based on case 2
max ((depth.snd) x) (depth (snd (maxDepthPair xs)))
= I.H.
max ((depth.snd) x) (maximum (map (depth.snd) xs))
= k
maximum((depth.snd) x : map (depth.snd) xs)
= m
maximum (map (depth.snd) (x:xs))