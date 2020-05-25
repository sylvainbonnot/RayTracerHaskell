module RTCTuple where

epsilon = 0.0001
epsilonsquared = epsilon * epsilon 

data RTCTuple a = RTCTuple {x, y, z, w ::a} deriving (Eq, Show)


tupleToList :: RTCTuple a -> [a]
tupleToList a = [x a, y a, z a, w a] 

build :: (Floating a)=> a -> a-> a-> a -> RTCTuple a
build x y z w = RTCTuple {x = x, y=y, z=z, w=w}

point :: (Floating a) => a -> a -> a -> RTCTuple a
point x y z = build x y z 1.0

vector :: (Floating a) => a -> a -> a -> RTCTuple a
vector x y z = build x y z 0.0

--origin = point 0. 0. 0.

isvector :: (Floating a, Eq a)=> RTCTuple a -> Bool
isvector a = w a == 0.0

ispoint :: (Floating a, Eq a)=> RTCTuple a -> Bool
ispoint a = w a == 1.0

square :: (Floating a)=>a -> a
square x = x * x

sqnorm :: (Floating a)=> RTCTuple a -> a
sqnorm a =  sum (map square (tupleToList a))

subtract :: (Floating a)=> RTCTuple a-> RTCTuple a-> RTCTuple a
subtract a b = build (x a - x b) (y a - y b) (z a - z b) (w a - w b)

add :: (Floating a)=>RTCTuple a -> RTCTuple a -> RTCTuple a
add a b = build (x a + x b) (y a + y b) (z a + z b) (w a + w b)
 

mults :: (Floating a)=> RTCTuple a->  a-> RTCTuple a
mults a scalar = build ((x a) * scalar) ((y a) * scalar) ((z a) * scalar) ((w a) * scalar) 


divs :: (Floating a)=> RTCTuple a->  a-> RTCTuple a
divs a scalar = build ((x a) / scalar) ((y a) / scalar) ((z a) / scalar) ((w a) / scalar)


neg :: (Floating a)=> RTCTuple a-> RTCTuple a
neg a = build c d e f  where c = negate (x a)
                             d = negate (y a)
                             e = negate (z a)
                             f = negate (w a) 

mag :: (Floating a)=> RTCTuple a ->  a
mag vec = sqrt  (sqnorm vec)

norm :: (Floating a)=> RTCTuple a-> RTCTuple a
norm vec = divs vec (mag vec)

safe_equal :: (Floating a, Ord a)=> RTCTuple a -> RTCTuple a-> a-> Bool
safe_equal c d eps= (sqnorm (RTCTuple.subtract c d)) <= eps


add_mults::(Floating a)=> RTCTuple a->RTCTuple a-> a->RTCTuple a
add_mults p v s = add p (mults v s)

dot::(Floating a)=> RTCTuple a->RTCTuple a-> a
dot a b = x a * x b + y a * y b + z a * z b

cross::(Floating a)=> RTCTuple a->RTCTuple a-> RTCTuple a
cross a b = vector (y a * z b - z a * y b)
                       (z a * x b - x a * z b)
                       (x a * y b - y a * x b)


reflect::(Floating a)=> RTCTuple a->RTCTuple a->RTCTuple a
reflect inv normal =
  add_mults inv normal s
  where in_dot_normal = dot inv normal
        s = -(2.0 * in_dot_normal)