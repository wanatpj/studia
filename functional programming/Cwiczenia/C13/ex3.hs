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

{------}
{- IO -}

digits::IO ()
digits = do
	c <- getChar
	if (c=='x') then return ()
	else do
		let str= if (isDigit c) then "-digit" else "-no digit"
		putStrLn str
		digits

readInt::String -> IO Int
readInt prompt= do
	putStrLn prompt
	str <- getLine
	case (readDec str) of
		(x:xs) -> return $ fst x
		[] 		-> do
			putStrLn "No parse."
			readInt prompt

evalIO::Context->Term-> IO Int
evalIO ctx t= runReaderT (evalIOM t) ctx

evalIOM::Term->ReaderT Context IO Int
evalIOM (Val x) = return x
evalIOM (Var c) = do
	ctx <- ask 
	let xM =Map.lookup c ctx
	if (isJust xM) 
		then return (fromJust xM) 
		else lift $ readInt $ "Podaj wartość dla "++[c]++":"

evalIOM (Plus t1 t2)= do
	v1 <- evalIOM t1
	v2 <- evalIOM t2
	return (v1+v2)

