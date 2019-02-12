module TC where

{- typ dla drzew binarnych w których kolejność dzieci nie ma znaczenia  -}
data CTree a = Leaf a | Node (CTree a) (CTree a)


{-
zaimplementuj predykat równości ctreeEq dla obiektów typu CTree 
-}

{- definicja klasy Eq (z Prelude)

class  Eq a  where
  (==), (/=)            :: a -> a -> Bool
  x /= y                =  not (x == y)

-}

--instance Eq a => Eq (CTree a) where
--	(==)  = ctreeEq

{-przykłady -}
tree1= (Leaf 7)
tree2= Node tree1 tree1
tree3= Node tree2 tree1
tree4= Node tree1 tree2
tree5= Node tree2 tree2
tree6= Node tree3 tree1
tree7= Node tree1 tree4

{- test -}
checkList= [(tree1, tree1), (tree1,tree2), (tree2, tree3), (tree3, tree4), (tree4,tree5), (tree5, tree6), (tree6, tree7)]
-- results: [True,False,False,True,False,False,True]

{- definiujemy własną parę -}
data MPairI = MPairI Integer Integer deriving (Show)

toMPair (x,y)= MPairI x y

{- dodaj predykat równości dla par (po współrzędnych) -}
--instance Eq MPairI  where
--	(MPairI x1 x2) == (MPairI y1 y2) = ...

{- definicja klasy Ord (z Prelude)

class  (Eq a) => Ord a  where
    compare              :: a -> a -> Ordering
    (<), (<=), (>), (>=) :: a -> a -> Bool
    max, min             :: a -> a -> a

    compare x y = if x == y then EQ
                  -- NB: must be '<=' not '<' to validate the
                  -- above claim about the minimal things that
                  -- can be defined for an instance of Ord:
                  else if x <= y then LT
                  else GT

    x <  y = case compare x y of { LT -> True;  _ -> False }
    x <= y = case compare x y of { GT -> False; _ -> True }
    x >  y = case compare x y of { GT -> True;  _ -> False }
    x >= y = case compare x y of { LT -> False; _ -> True }

        -- These two default methods use '<=' rather than 'compare'
        -- because the latter is often more expensive
    max x y = if x <= y then y else x
    min x y = if x <= y then x else y


-}

{- uczyń MPairI instancją klasy Ord, porządkując najpierw po sumie współrzędnych, a potem po pierwszej z nich -}
--instance Ord MPairI where 
--	compare (MPairI x1 x2) (MPairI y1 y2) = ...

{- przeczytaj w dokumentacji opis Maybe -}

mlength = length

mhead::[a]->Maybe a
mhead list
	| mlength list == 0  	= Nothing
	| otherwise		= Just$head list


{- dlaczego powyższa implementacja jest zła? -}

{- definiujemy własne numerale -}
data PNumber = Zero | Succ PNumber deriving (Eq)

{- zaimplementuj funkcję zwracającą długość listy na PNumber -}
--mlength::[a]->PNumber
{- przepisz funkcję mhead używając PNumbers -}


{- uczyń PNumber instancją klasy Num -}
{- uczyń PNumber instancją klasy Show -}
{- uczyń CTree instancją klasy Show -}


