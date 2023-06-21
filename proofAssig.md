Please give a formal equational reasoning proof showing that

mapFoo f . mapFoo g = mapFoo (f . g)

The proof should take the shape of a series of chained-together equalities of Haskell programs. Each equality should be justified by either the letter of a definition or should be marked as (I.H.) if it follows from the use of an induction hypothesis. Every induction hypothesis should be clearly stated.


You may use the following definitions

data Foo a = Zero | One a | Rec (Foo a) (Foo a)

(.) f g a = f (g a) -- (a)

mapFoo f Zero = Zero -- (b)

mapFoo f (One a) = One (f a) -- (c)

mapFoo f (Rec l r) = Rec (mapFoo f l) (mapFoo f r) -- (d)
Claim:  for all t = Foo a
(mapFoo f . mapFoo g) t = mapFoo (f . g) t 

We will prove this by induction on t, distinguishing the cases
t = Zero and t = One a and t = Rec (Foo a) (Foo a) 

So `Foo a` could be `Zero` or `One a` or `Rec (Foo a) (Foo a)`

Base Case 1 : t = Zero 
(mapFoo f . mapFoo g) Zero
= a
mapFoo f (mapFoo g Zero)
= b
mapFoo f Zero
= b
Zero

Zero
= b
mapFoo (f.g) Zero

Base Case 2: t = One a
(mapFoo f . mapFoo g) (One a)
= a 
mapFoo f (mapFoo g (One a))
= c
mapFoo f (One (g a))
= c
One (f (g a))
= a
One ((f . g) a)

One ((f . g) a)
= c
mapFoo (f.g) (One a)

We assume the induction hypotheses that

(mapFoo f . mapFoo g) t1 = (mapFoo (f . g)) t1 -- (I.H.1)
(mapFoo f . mapFoo g) t2 = (mapFoo (f . g)) t2 -- (I.H.2)

We should prove the inductive step: 
(mapFoo f . mapFoo g) (Rec t1 t2) = mapFoo (f . g) (Rec t1 t2)

Proof:

(mapFoo f . mapFoo g) (Rec t1 t2) 
= a
mapFoo f ( mapFoo g (Rec t1 t2) )
= d
mapFoo f (Rec (mapFoo g t1) (mapFoo g t2))
= d 
Rec (mapFoo f (mapFoo g t1)) (mapFoo f (mapFoo g t2))
= a
Rec ((mapFoo f . mapFoo g) t1) ((mapFoo f . mapFoo g) t2)
= I.H.1 
Rec (mapFoo (f . g) t1) (mapFoo f (mapFoo g t2))
= I.H.2
Rec (mapFoo (f . g) t1) (mapFoo (f . g) t2)
= d
mapFoo (f . g) (Rec t1 t2)

Our claim follows by induction on t