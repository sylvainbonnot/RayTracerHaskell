module Ray where

import RTCTuple

data Ray a = Ray {origin::RTCTuple a, direction::RTCTuple a} deriving (Eq, Show)

build::RTCTuple a-> RTCTuple a -> Ray a
build or dir = Ray {origin= or, direction = dir}

position:: Ray Float -> Float -> RTCTuple Float
position ray t = RTCTuple.add_mults (origin ray) (direction ray) t