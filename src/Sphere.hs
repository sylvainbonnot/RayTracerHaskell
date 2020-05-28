module Sphere where

import Matrix
import RTCTuple
import Transform

data Sphere = Sphere {sphereId :: Int}
  deriving (Eq,Show)