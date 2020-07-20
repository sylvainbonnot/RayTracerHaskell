


# A ray tracer from scratch, in Haskell

This project implements a ray tracer, following the excellent book [The Ray Tracer challenge](http://raytracerchallenge.com), by Jamis Buck.

- [x] Tuples, Points and Vectors
- [x] Drawing on a canvas
- [x] Matrices
- [x] Matrix Transformations
- [x] Ray-sphere intersections
- [x] Light and Shading
- [ ] Making a scene
- [ ] Shadows
- [ ] Planes
- [ ] Patterns
- [ ] Reflection and Refraction
- [ ] Cubes
- [ ] Cylinders
- [ ] Groups
- [ ] Triangles
- [ ] Constructive Solid Geometry
- [ ] Next Steps

## Tuples, Points and Vectors

Points and vectors are represented as 4-tuples of Floats, with the difference that points always end with a 1 as last coordinate, like (4, -4, 3, 1), and vectors always end with a 0.

```haskell
data RTCTuple a = RTCTuple {x, y, z, w ::a} deriving (Eq, Show)
```

This chapter implements basic arithmetic operations on those tuples: addition, subtraction, multiplication by a scalar, approximate equality, dot product, cross product, etc...

## Drawing on a canvas

Colors are introduced as (r,g,b) tuples that are Num instances, allowing some simple operations on colors:

```haskell
newtype Color = Color (Float, Float, Float) deriving (Eq, Show)

instance Num Color where
    Color (r1, g1, b1) + Color (r2, g2, b2) = Color (r1 + r2, g1 + g2, b1 + b2)
    Color (r1, g1, b1) - Color (r2, g2, b2) = Color (r1 - r2, g1 - g2, b1 - b2)
    _ * _ = undefined
    abs _ = undefined
    signum _ = undefined
    fromInteger _ = undefined
    negate (Color (r, g, b)) = Color (-r, -g, -b)
```

From there the notion of **canvas** is defined as a rectangular grid of pixels:
```haskell
-- Pixel matrix
type PixelMatrix = Array (Int, Int) Color

-- Canvas
newtype Canvas = Canvas PixelMatrix deriving (Show)

```
One can write a pixel at a given position
```haskell
writePixelAt :: Canvas -> Int -> Int -> Color -> Canvas
writePixelAt (Canvas m) i j c = if (outsideCanvas i j (Canvas m)) then (Canvas m) 
    else Canvas (m // [((i,j), c)])
```
and also write a PPM file from a canvas, to visualize our work:
```haskell
ppmFromCanvas :: Canvas -> String
ppmFromCanvas c = unlines ("P3" : [show w ++ " " ++ show h ] ++ ["255"] ++ pixelData c)
    where
        w = width c
        h = height c
```







