module Main where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss
import System.Random

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

listToCoords :: Integer -> [Point]
listToCoords a = listify 0 $ generateCollatzList a
  where
    listify :: Integer -> [Integer] -> [Point]
    listify _ [] = []
    listify a (x:xs) =
      (fromIntegral a, fromIntegral x):listify (a + 10) xs


-- Graphics Generation
drawPoints :: [(Float, Float)] -> Picture
drawPoints pts = Pictures $ map drawDot pts
  where
    drawDot (x, y) = translate x y $ color red $ circleSolid 4

main :: IO ()
main = do
  display
    (InWindow "test" (40,40) (40,40))
    (makeColorI 255 255 255 255)
    (Pictures $
      [line $ listToCoords x | x <- [1..1000]]
    )
