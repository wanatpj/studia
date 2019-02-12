module Solution where
import System.Random
newtype Randomised a = Rand (StdGen->(a, StdGen))
instance Monad Randomised where
   Rand b >>= f  =
      Rand (\seed ->
         let (arr, rSeed) = b seed
             Rand mon     = f arr in
               mon rSeed)
   return k = Rand (\seed -> (k, seed))

runR (Rand rf) = rf

getRE :: [a] -> Randomised a

getRE a = Rand (\seed ->
    let (elem, nSeed) = next seed in (a !! (elem `mod` (length a)), nSeed))

solution :: (Ord a) => [a] -> Randomised [a]
solution [] = return []
solution a  =
    (getRE a) >>=
    (\elem ->
        return [filter (<elem) a, filter (==elem) a, filter (>elem) a]) >>=
    (\x -> Rand(\seed ->
        ([fst $ (runR $ solution $ head x) seed] ++ tail x, seed))) >>=
    (\x -> Rand(\seed ->
        (take 2 x ++ [fst $ (runR $ solution $ x !! 2) seed], seed))) >>=
    (\x -> return $ concat $ x)
