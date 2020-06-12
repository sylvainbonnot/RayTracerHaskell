module Sphere  (Sphere, sphere, Object, Intersection, Intersections, intersect, setTransform, normalAt, normalAtObj)where

import Matrix
import RTCTuple
import Transform
import Ray
import Intersection
import Material
import Data.List (sort)

data Sphere = Sphere {sphereId::Int, transform::Matrix Float, material:: Material}
  deriving (Eq,Show)

data Object = Object Sphere  deriving (Eq, Show)

sphere:: Sphere
sphere = Sphere {sphereId = 0, Sphere.transform = Matrix.identity, material = defaultMaterial}


data Intersection = Intersection {t:: Float, object:: Object} deriving (Eq, Show)
instance Ord Intersection where
    compare x y = compare (t x) (t y)


type Intersections = [Intersection]

hit:: Intersections -> Maybe Intersection
hit xs = if (length res)>=1 then Just (head res) else Nothing
            where res = sort [x | x <- xs, (t x)>=0.0]


intersect::Sphere -> Ray Float -> Intersections
intersect s ray = if fst d <0.0 then []::Intersections
                  else [Intersection {t = t1, object = Object s}, Intersection{t = t2, object = Object s}]
                    where ray2 = Ray.transform ray (Matrix.inverse (Sphere.transform s)) 
                    	  d = discriminant ray2
                          [t1, t2] = snd d

setTransform:: Sphere -> Matrix Float -> Sphere
setTransform s@Sphere {sphereId = i, Sphere.transform = t} m = s {sphereId = i, Sphere.transform = m}

discriminant::Ray Float -> (Float, [Float])
discriminant ray = (disc, [t1, t2])
    where sphere_to_ray = RTCTuple.subtract (origin ray) (RTCTuple.point 0.0 0.0 0.0) 
          a = RTCTuple.dot (direction ray) (direction ray)
          b = 2.0 * RTCTuple.dot (direction ray) sphere_to_ray
          c = (-1.0) + RTCTuple.dot sphere_to_ray sphere_to_ray
          disc = b*b -4.0*a*c
          t1 = (-b-sqrt disc)/(2.0*a)
          t2 = (-b+sqrt disc)/(2.0*a)


normalAt:: Sphere-> RTCTuple Float -> RTCTuple Float
normalAt s p = norm (vector x y z)
             where sinv = Matrix.inverse (Sphere.transform s)
             	   objectPoint = tmult sinv p
                   objectNormal = (RTCTuple.subtract objectPoint (RTCTuple.point 0.0 0.0 0.0))::RTCTuple Float
                   worldNormal = (tmult (Matrix.transpArray sinv) objectNormal)::RTCTuple Float
                   [x, y, z, _] = tupleToList worldNormal


normalAtObj:: Object -> RTCTuple Float -> RTCTuple Float
normalAtObj (Object s) p = Sphere.normalAt s p


