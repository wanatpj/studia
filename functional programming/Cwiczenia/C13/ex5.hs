module Main where


import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State

import Data.Maybe
import Data.Char

import Numeric (readDec)

import qualified Data.Map as Map


data Term = Val Int | Var Char | Plus Term Term deriving (Show)
t1=Plus (Var 'x') (Val 7)
t2=Plus (Var 'x') (Plus (Val 7) (Var 'z'))

type Subs = Map.Map Char Term
m1= (Map.fromList [('y',Val 1),('x',Val 9)])::Subs

type Context= Map.Map Char Int
c1= (Map.fromList [('y', 1),('x',9)])::Context


readInt::String -> IO Int
readInt prompt= do
	putStrLn prompt
	str <- getLine
	case (readDec str) of
		(x:xs) -> return $ fst x
		[] 		-> do
			putStrLn "No parse."
			readInt prompt


{-----------------------}
{- transformer: StateT -}

evalSIO::Context->Term -> IO Int
evalSIO ctx t = do
	(r,ctx')<- runStateT (evalSIOM t) ctx
	return r

evalSIOM::Term -> StateT Context IO Int
evalSIOM (Val x) = return x
evalSIOM (Var c) = do
	ctx <- get
	let xM =Map.lookup c  ctx
	if (isJust xM) 
		then return (fromJust xM) 
		else do
			v <-lift $ readInt $ "Podaj wartość dla "++[c]++":"
			put $ Map.insert c v ctx
			evalSIOM (Var c)

evalSIOM (Plus t1 t2)= do
	v1 <- evalSIOM t1
	v2 <- evalSIOM t2
	return (v1+v2)

t4= Plus (Var 'x') (Plus (Plus (Var 'y') (Var 'x')) (Plus (Var 'y') (Plus (Var 'z') (Var 'x'))))
c4= (Map.fromList [('y',1)])::Context

{- evalSIO c4 t4 , podaj kolejno 10 i 100, powinno wyjść 132 -}

