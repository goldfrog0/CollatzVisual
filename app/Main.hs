module Main where

-- Function is safe if Collatz is true
collatzCheck :: Integer -> Bool
collatzCheck 1 = True
collatzCheck n
  | odd n = collatzCheck $ 3*n + 1
  | otherwise = collatzCheck $ n `div` 2

collatzList :: (Bool, [Integer]) -> (Bool, [Integer])
collatzList (False, a)
  | n == 1 = (True, a)
  | odd n  = collatzList (False, a ++ [3*n + 1])
  | even n = collatzList (False, a ++ [n `div` 2])
  where n = last a

main :: IO ()
main = putStrLn "Hello, Haskell!"
