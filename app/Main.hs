module Main where

-- Function is safe if Collatz is true
collatzCheck :: Integer -> Bool
collatzCheck 1 = True
collatzCheck n
  | odd n = collatzCheck $ 3*n + 1
  | otherwise = collatzCheck $ n `div` 2

collatzList :: (Bool, [Integer]) -> (Bool, [Integer])
collatzList (_, []) = (False, [])
collatzList l@(True, a)
  | head a == 1 = l
  | otherwise = collatzList (False, a)
collatzList (False, a)
  | n == 1 = (True, a)
  | head a <= 0 = (False, a)
  | odd n  = collatzList (False, 3*n + 1 : a)
  | even n = collatzList (False, n `div` 2 : a)
  where n = head a
collatzList _ = error "not intended input"

generateCollatzList :: Integer -> [Integer]
generateCollatzList a = snd . collatzList $ (False, [a])



main :: IO ()
main = putStrLn "Hello, Haskell!"
