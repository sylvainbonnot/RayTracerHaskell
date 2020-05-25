module Canvas  where

import Data.Array
import Color

-- Pixel matrix
type PixelMatrix = Array (Int, Int) Color

-- For now, let's try an immutable Array type and see how that works.
newtype Canvas = Canvas PixelMatrix deriving (Show)

createCanvas :: Int -> Int -> Canvas
createCanvas x y = Canvas (array ((0,0), (x-1,y-1)) [ ((i,j), 
                Color (0,0,0)) | i <- [0..x-1], j <- [0..y-1]] )


width :: Canvas -> Int
width (Canvas m) = 1 + fst ( snd (bounds m))

height :: Canvas -> Int
height (Canvas m) = 1 + snd ( snd (bounds m))

transpose :: Array (Int,Int) a -> Array (Int,Int) a
transpose arr = ixmap (swap lo, swap hi) swap arr 
    where swap (r,c) = (c,r)
          (lo, hi) = bounds arr


allPixels :: Canvas -> [Color]
allPixels (Canvas m) = elems (transpose m)


pixelAt :: Canvas -> Int -> Int -> Color
pixelAt (Canvas m) i j = m!(i, j)

outsideCanvas:: Int -> Int -> Canvas -> Bool
outsideCanvas i j c = i<0 || i>= (width c) || j<0 || j>= (height c)

writePixelAt :: Canvas -> Int -> Int -> Color -> Canvas
writePixelAt (Canvas m) i j c = if (outsideCanvas i j (Canvas m)) then (Canvas m) else Canvas (m // [((i,j), c)])

ppmFromCanvas :: Canvas -> String
ppmFromCanvas c = unlines ("P3" : [show w ++ " " ++ show h ] ++ ["255"] ++ pixelData c)
    where
        w = width c
        h = height c

clamp :: Int-> Float -> Int
clamp maxval value = 
    --let rescaled = (((fromIntegral maxval) *) . max 0.0 . min 1.0) value in
    --floor (rescaled+0.5) 
    max 0 ( min 255  (   floor(value * (fromIntegral maxval) + 0.5)  ))

group :: Int -> [a] -> [[a]]
group _ [] = []
group n xs =
  let (xs0,xs1) = splitAt n xs
  in  xs0 : group n xs1



pixelData :: Canvas -> [String]
pixelData c = map unwords ( group (15) $ concat [  map ((show . (clamp 255)) . ($ p)) [red, green, blue] | p <- allPixels c])

setAllPixelsTo :: Canvas -> Color -> Canvas
setAllPixelsTo (Canvas m) cl = Canvas (m // [ (i, cl) | i <- indices m])