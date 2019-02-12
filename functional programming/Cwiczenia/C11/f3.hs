module F3 where 

--inc= (\n -> n+1) -- zamiast tego możemy dołączyć w ghci cały moduł f1: ":add f1.hs"


twice f x = f(f(x))	-- nawiasy dookola x sa zbyteczne, ale tak wyglada bardziej znajomo

fsum f 1= f(1)
fsum f n= f(n)+ fsum f (n-1)

{- używając fsum napisz termy obliczające:
	\n -> liczba liczb względnie pierwszych z n, mniejszych od n
	\n -> sumę i^i po wszystkich i < n
	\n -> największa liczba m taka, że 7^m dzieli n!
-}

cK x y = x	-- kombinator K (przypomnienie)
cS a b c = (a c) (b c)	-- kombinator S
{-
sprobuj przewidzieć wyniki 
	cS (+) (+ 7) 2
	cS (+) (+ 7) 3
	cS (+) (+ 8) 2
	cS (*) (+ 8) 2
	cS cK cK 42
	cS cK cK 43
-}


{- 
przeanalizuj poniższy kod. Jaki będzie wynik wywołania "fixp foo 7" ?
-}
fixp f x = f (fixp f) x

foo f n= if n==0 then 0 else n+ (f (n-1))

{- napisz term który przekształca liczbę na numeral, oraz drugi który przekształca numeral na liczbę. Zdefiniuj następujące operacje na numeralach  (bez używania operacji na liczbach)
	inc, add, mult, pow, dec (sprobuj zdefiniowac dec tak jak w nietypowanym rachunku lambda)
	predykat parzystości
-}

