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

{------------------------}
{- transformer: ReaderT -}

eval'::Context -> Term ->Maybe Int
eval' ctx t = runReaderT (evalM' t) ctx

evalM':: Term -> ReaderT Context  Maybe Int
evalM' (Val x) = return x
evalM' (Var c) = do
	ctx <- ask
	lift (Map.lookup c ctx)


evalM' (Plus t1 t2)= do
	v1 <- evalM' t1
	v2 <- evalM' t2
	return (v1+v2)

