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

{---------}
{- State -}
evalS::Context->Term-> Int
evalS ctx t = fst $ runState (evalSM t) (ctx,0)

evalSM::Term-> State (Context,Int) Int
evalSM (Val x) = return x
evalSM (Var c) = do
	(ctx,nn) <- get
	let xM =Map.lookup c  ctx
	if (isJust xM) 
		then return (fromJust xM) 
		else do
			put $ (Map.insert c nn ctx,1+nn)
			evalSM (Var c)

evalSM (Plus t1 t2)= do
	v1 <- evalSM t1
	v2 <- evalSM t2
	return (v1+v2)

t3= Plus (Var 'x') (Plus (Var 'y') (Plus (Var 'y') (Var 'z')))
c3= Map.empty

{- evalST c3 t3 == 4 -}

