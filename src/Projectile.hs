module Projectile
  (
    Environment(..)
    ,Projectile(..)
    ,tick
  ) where

import RTCTuple

data Environment = Environment { gravity:: RTCTuple Double, wind :: RTCTuple Double} deriving (Eq, Show)

data Projectile = Projectile { position:: RTCTuple Double, velocity :: RTCTuple Double} deriving (Eq, Show)

tick :: Environment -> Projectile -> Projectile
tick environment p0 = Projectile { position = add (position p0)  (velocity p0), velocity = add (add (velocity p0) ( gravity environment))  
                    (wind environment) }