module Color (
    Color(..)
    ,red
    ,green
    ,blue
    ,mults
    ,mult
    , black
    
) where

--import Data.Array


newtype Color = Color (Float, Float, Float) deriving (Eq, Show)

first :: (Float, Float, Float) -> Float
first (t1, _, _) = t1

second :: (Float, Float, Float) -> Float
second (_, t2, _) = t2

third :: (Float, Float, Float) -> Float
third (_, _, t3) = t3


red :: Color -> Float
red (Color t) = first t

green :: Color -> Float
green (Color t) = second t

blue :: Color -> Float
blue (Color t) = third t

black::Color
black = Color (0.0, 0.0, 0.0)

white::Color
white = Color (1.0, 1.0, 1.0)

colorToList::Color -> [Float]
colorToList (Color (r,g,b)) = [r, g, b]


instance Num Color where
    Color (r1, g1, b1) + Color (r2, g2, b2) = Color (r1 + r2, g1 + g2, b1 + b2)
    Color (r1, g1, b1) - Color (r2, g2, b2) = Color (r1 - r2, g1 - g2, b1 - b2)
    _ * _ = undefined
    abs _ = undefined
    signum _ = undefined
    fromInteger _ = undefined
    negate (Color (r, g, b)) = Color (-r, -g, -b)

mult :: Color -> Color -> Color
mult (Color (r1, g1, b1))  (Color (r2, g2, b2)) = Color (r1 * r2, g1 * g2, b1 * b2) 

mults:: Color-> Float -> Color
mults (Color (r1, g1, b1)) a= Color (a * r1, a * g1, a * b1)
