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


{----------------}
{- triple stack -}

evalSWIO::Context->Term -> IO (Int,String)
evalSWIO ctx t = do
	(r,ctx') <- runStateT (runWriterT (evalSWIOM t)) ctx
	return r

--evalSWIOM::Term -> WriterT [Char] (StateT Context IO) Int
evalSWIOM (Val x) = return x
evalSWIOM (Var c) = do
	ctx <- get
	let xM =Map.lookup c  ctx
	if (isJust xM) 
		then do
			tell $ [c]++" found in context. "
			return (fromJust xM) 
		else do
			v <-lift $ lift $ readInt $ "Podaj wartość dla "++[c]++":"
			tell $ [c]++" added to context. "
			put $ Map.insert c v ctx
			evalSWIOM (Var c)

evalSWIOM (Plus t1 t2)= do
	v1 <- evalSWIOM t1
	v2 <- evalSWIOM t2
	return (v1+v2)

{- evalSWIO c4 t4 , podaj kolejno 10 i 100, powinno wyjść:
(132,"x added to context. x found in context. y found in context. x found in context. y found in context. z added to context. z found in context. x found in context. ")
-}

