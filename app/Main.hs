module Main where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss
import Graphics.EasyPlot
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

-- Convert collatzList to vector chain

-- convert list, based on if its even or odd
evens :: [Integer] -> [Bool]
evens = map even

nextPoint :: (Float, Float) -> (Float, Float) -> Bool -> (Float, Float)
nextPoint (x1, y1) (x2, y2) b
  | b = (cos theta + x2, sin theta + y2)
  | otherwise = (cos theta' + x2, sin theta' + y2)
  where theta = atan2 (y2 - y1) (x2 - x1) + (pi/24)
        theta' = atan2 (y2 - y1) (x2 - x1) - (pi/24)

collatzPath :: Integer -> [Point]
collatzPath a = makePath (0,0) (cos pi/6, sin pi/6) $ evens $ generateCollatzList a
  where makePath :: (Float, Float) -> (Float, Float) -> [Bool] -> [(Float, Float)]
        makePath (_, _) (_, _) [] = []
        makePath p1 p2 (x:xs) = nextPoint p1 p2 x : makePath p2 (nextPoint p1 p2 x) xs


-- Graphics Generation

listToCoords :: Integer -> [Point]
listToCoords a = listify 0 $ generateCollatzList a
  where
    listify :: Integer -> [Integer] -> [Point]
    listify _ [] = []
    listify a (x:xs) =
      (fromIntegral a, fromIntegral x):listify (a + 10) xs

drawPoints :: [(Float, Float)] -> Picture
drawPoints pts = Pictures $ map drawDot pts
  where
    drawDot (x, y) = translate x y $ color red $ circleSolid 4


test :: IO Bool
test = do plot' [Interactive] X11 $ [Data2D [Title "Test Plot", Style Lines] [] $ collatzPath a | a <- [1..2000]]

test1 :: IO Bool
test1 = plot X11 $ [Data2D [Title "Sample Data"] [] $ collatzPath a | a <- [1..100]]

main :: IO ()
main = do
  display
    (InWindow "test" (40,40) (40,40))
    (makeColorI 255 255 255 255)
    (Pictures $
      [line $ collatzPath x | x <- [1..1000]]
    )
