module LightSpec where

import Test.Hspec

import RTCTuple
import Light
import Color

spec :: Spec
spec = do
  describe "Light features" $ do
    describe "A point light has a position and intensity" $ do
      let c = Color (1.0, 1.0, 1.0)
      let p = RTCTuple.point 0.0 0.0 0.0 
      let light = pointLight c p

      it "light.position = position" $
        RTCTuple.equal (position light) p `shouldBe` True
      it "light.intensity = intensity" $
        (intensity light) == c `shouldBe` True
      