module Ray where

import RTCTuple
import Matrix 
import Transform

data Ray a = Ray {origin::RTCTuple a, direction::RTCTuple a} deriving (Eq, Show)

build::RTCTuple a-> RTCTuple a -> Ray a
build or dir = Ray {origin= or, direction = dir}

position:: Ray Float -> Float -> RTCTuple Float
position ray t = RTCTuple.add_mults (origin ray) (direction ray) t

translate:: Ray Float -> Matrix Float ->Ray Float
translate ray trans = Ray {origin = new_origin, direction = direction ray}
    where new_origin = tmult trans (origin ray) 

scale:: Ray Float -> Matrix Float ->Ray Float
scale ray trans = Ray {origin = new_origin, direction = new_direction}
    where new_origin = tmult trans (origin ray)
          new_direction = tmult trans (direction ray)

transform:: Ray Float -> Matrix Float ->Ray Float
transform ray trans = Ray {origin = new_origin, direction = new_direction}
    where new_origin = tmult trans (origin ray)
          new_direction = tmult trans (direction ray)