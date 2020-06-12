module MaterialSpec where

import Test.Hspec

import RTCTuple
import Light
import Color
import Material

spec :: Spec
spec = do
  describe "Material features" $ do
    describe "The default material" $ do
      let m = defaultMaterial
      it "m.color= Color (1,1,1)" $
        (Color (1.0, 1.0, 1.0)) ==(color m) `shouldBe` True
      it "m.ambient = 0.1" $
        (ambient m) == 0.1 `shouldBe` True

      it "m.diffuse = 0.9" $
        (diffuse m) == 0.9 `shouldBe` True
      it "m.specular = 0.9" $
        (specular m) == 0.9 `shouldBe` True
      it "m.shininess = 200.0" $
        (shininess m) == 200.0 `shouldBe` True

    describe "Lighting with the eye between the light and the surface" $ do
      let m = defaultMaterial
      let pos = RTCTuple.point 0.0 0.0 0.0
      let eyev = RTCTuple.vector 0.0 0.0 (-1.0)
      let normalv = RTCTuple.vector 0.0 0.0 (-1.0)
      let light1 = pointLight (Color (1.0,1.0, 1.0 )) (RTCTuple.point 0.0 0.0 (-10.0))
      let result = lighting m light1 pos eyev normalv

      it "result = Color (1.9,1.9,1.9)" $
        (Color (1.9, 1.9, 1.9)) ==result `shouldBe` True
      
    describe "Lighting with the eye between light and surface, eye offset 45deg" $ do
      let m = defaultMaterial
      let pos = RTCTuple.point 0.0 0.0 0.0
      let eyev = RTCTuple.vector 0.0 ((sqrt 2)/2.0) (-(sqrt 2)/2.0)
      let normalv = RTCTuple.vector 0.0 0.0 (-1.0)
      let light1 = pointLight (Color (1.0,1.0, 1.0 )) (RTCTuple.point 0.0 0.0 (-10.0))
      let result = lighting m light1 pos eyev normalv

      it "result = Color (1.,1.,1.)" $
        (Color (1.0, 1.0, 1.0)) ==result `shouldBe` True

    describe "Lighting with the eye opposite surface, light offset 45deg" $ do
      let m = defaultMaterial
      let pos = RTCTuple.point 0.0 0.0 0.0
      let eyev = RTCTuple.vector 0.0 0.0 (-1.0)
      let normalv = RTCTuple.vector 0.0 0.0 (-1.0)
      let light2 = pointLight (Color (1.0,1.0, 1.0 )) (RTCTuple.point 0.0 10.0 (-10.0))
      let result = lighting m light2 pos eyev normalv
      let expected = Color (0.7363961, 0.7363961, 0.7363961)

      it "result = Color (0.7364, 0.7364, 0.7364)" $
        result==expected `shouldBe` True

    describe "Lighting with eye in the path of the reflection vector" $ do
      let m = defaultMaterial
      let pos = RTCTuple.point 0.0 0.0 0.0
      let eyev = RTCTuple.vector 0.0 (-(sqrt 2)/2.0) (-(sqrt 2)/2.0)
      let normalv = RTCTuple.vector 0.0 0.0 (-1.0)
      let light = pointLight (Color (1.0,1.0, 1.0 )) (RTCTuple.point 0.0 10.0 (-10.0))
      let result = lighting m light pos eyev normalv
      let expected = Color (1.6363853, 1.6363853, 1.6363853) 

      it "result = Color (1.6363853, 1.6363853, 1.6363853)" $
        result==expected `shouldBe` True

    describe "Lighting with the light behind the surface" $ do
      let m = defaultMaterial
      let pos = RTCTuple.point 0.0 0.0 0.0
      let eyev = RTCTuple.vector 0.0 0.0 (-1.0)
      let normalv = RTCTuple.vector 0.0 0.0 (-1.0)
      let light = pointLight (Color (1.0,1.0, 1.0 )) (RTCTuple.point 0.0 0.0 10.0)
      let result = lighting m light pos eyev normalv
      let expected = Color (0.1, 0.1, 0.1) 

      it "result = Color (0.1, 0.1, 0.1)" $
        result==expected `shouldBe` True