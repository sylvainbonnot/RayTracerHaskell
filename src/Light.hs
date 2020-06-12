module Light where


import Color 
import RTCTuple

data Light = Light {intensity::Color, position::RTCTuple Float}

pointLight:: Color -> RTCTuple Float -> Light
pointLight color pt = Light {intensity= color, position = pt}