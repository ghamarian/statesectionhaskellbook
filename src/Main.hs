{-# LANGUAGE InstanceSigs #-}
module Main where

import Control.Applicative (liftA3) 
import Control.Monad (replicateM) 
import Control.Monad.Trans.State  hiding (get, put, exec, modify)


import System.Random

main :: IO ()
main = do
  putStrLn "hello world"

data Die =
    DieOne
      | DieTwo
      | DieThree
      | DieFour
      | DieFive
      | DieSix
      deriving (Eq, Show)

intToDie :: Int -> Die 
intToDie n =
   case n of
      1 -> DieOne
      2 -> DieTwo
      3 -> DieThree
      4 -> DieFour
      5 -> DieFive
      6 -> DieSix
      -- Use this tactic _extremely_ sparingly.
      x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollDie :: State StdGen Die 
rollDie = state $ do
   (n, s) <- randomR (1, 6) 
   return (intToDie n, s)

rollsToGetTwenty :: StdGen -> Int 
rollsToGetTwenty g = go 0 0 g
         where go :: Int -> Int -> StdGen -> Int 
               go sum count gen
                  | sum >= 20 = count 
                  | otherwise = let (die, nextGen) = randomR (1, 6) gen 
                                in go (sum + die) (count + 1) nextGen



rollsToGetTwentyH :: Int -> StdGen -> (Int, [Die])
rollsToGetTwentyH limit g = go 0 0 [] g 
         where go :: Int -> Int -> [Die] -> StdGen -> (Int, [Die])
               go sum count dieList gen
                  | sum >= limit = (count, dieList)
                  | otherwise = let (die, nextGen) = randomR (1, 6) gen 
                                in go (sum + die) (count + 1) (intToDie die : dieList) nextGen



newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
   fmap :: (a -> b) -> Moi s a -> Moi s b
   fmap f ma = Moi $ \s -> let (a, b) = runMoi ma s in (f a, b) 

instance Applicative (Moi s) where
   pure a = Moi $ \s -> (a, s)
   (Moi f) <*> (Moi a) = Moi $ \s -> let  (g, s') = f s 
                                          (k, s'') = a s'
                                in (g k, s'') where

instance Monad (Moi s) where
  return = pure
  (Moi ma) >>= amb = Moi $ \s -> let (a, s') = ma s in
                                  runMoi (amb a) s'

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
		     |n`mod`5 ==0="Buzz" 
           |n`mod`3 ==0="Fizz" 
           | otherwise = show n

get :: State s s 
get = state $ \s -> (s, s)

put :: s -> State s () 
put s = state $ \r -> ((), s)

exec :: State s a -> s -> s
exec sa  = snd . (runState sa) 

eval :: State s a -> s -> a 
eval sa =  fst . (runState sa)

modify :: (s -> s) -> State s () 
modify f = state $ \s -> ((), f s)
