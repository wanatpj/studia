module Main where


import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State

import Data.Maybe
import Data.Char

import Numeric (readDec)

import qualified Data.Map as Map


{----------}
{- Writer -}


col::Int -> Writer [Char] Int
col n = if (n `mod` 2 == 0) 
		then tell "div." 	>> return (n `div` 2)
		else tell "3n." 	>> return (3*n+1)

colN n 0= return n
colN n k= do 
	n' <- col n
	colN n' (k-1)

r= runWriter (colN 7 7)


{----------}
{- Reader -}

data Term = Val Int | Var Char | Plus Term Term deriving (Show)
t1=Plus (Var 'x') (Val 7)
t2=Plus (Var 'x') (Plus (Val 7) (Var 'z'))

type Subs = Map.Map Char Term
m1= (Map.fromList [('y',Val 1),('x',Val 9)])::Subs

applySubst::Subs -> Term -> Term
applySubst ss t= runReader (applySubstM t) ss	


applySubstM (Val x) = return (Val x)

applySubstM (Var c) = do
	s <- ask 
	let mv = Map.lookup c s	
{-	mv <- asks (Map.lookup c) -}
	return (if (isJust mv) then fromJust mv else (Var c))

applySubstM (Plus t1 t2)= do
	t1' <- applySubstM t1
	t2' <- applySubstM t2
	return (Plus t1' t2')

type Context= Map.Map Char Int
c1= (Map.fromList [('y', 1),('x',9)])::Context

eval::Context -> Term ->Maybe Int
eval ctx t = runReader (evalM t) ctx

evalM::Term -> Reader Context  (Maybe Int)
evalM (Val x) = return (Just x)
evalM (Var c) = asks (Map.lookup c) 
evalM (Plus t1 t2)= do
	v1M <- evalM t1
	v2M <- evalM t2
	return (do
		v1 <- v1M
		v2 <- v2M
		return (v1+v2)
		)
