module Material where

import Color 
import RTCTuple
import Light

data Material = Material {color :: Color ,
                         ambient :: Float, diffuse::Float, specular :: Float,
                         shininess :: Float
                  } deriving (Show, Eq)


defaultMaterial:: Material
defaultMaterial = Material{color = Color (1.0, 1.0, 1.0),
                         ambient = 0.1, diffuse=0.9, specular =0.9,
                         shininess = 200.0
                  }

lighting::Material->Light->RTCTuple Float->RTCTuple Float->RTCTuple Float-> Color
lighting mat light p eyev normalv = let effectiveColor = Color.mult (color mat) (intensity light) in
                                     let lightv = norm (RTCTuple.subtract (position light) p) in
                                     let amb = Color.mults effectiveColor (ambient mat) in 
                                     let lightDotNormal = dot lightv normalv in 
                                     let (diff, spec) = if (lightDotNormal<0.0) then (black, black) 
                                     	                     else (d, s)
                                     	                     where d = Color.mults effectiveColor (diffuse mat * lightDotNormal)
                                                                   reflectv = reflect (RTCTuple.mults lightv (-1.0)) normalv
                                                                   reflectDotEye = dot reflectv eyev
                                                                   factor = reflectDotEye ** (shininess mat)
                                                                   s = if (reflectDotEye <=0.0) then black
                                                                   	          else Color.mults (intensity light) (specular mat*factor)
                                     in 
                                    amb + diff + spec

