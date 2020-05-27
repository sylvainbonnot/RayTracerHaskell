module TransformSpec where

import Test.Hspec
import Matrix
import RTCTuple
import Data.Array
import Transform

spec :: Spec
spec = describe "Transform" $ do

    describe "Constructing and inspecting a 4x4 Matrix" $ do
        let transform = Transform.translation 5.0 (-3.0) 2.0

        let p = RTCTuple.point (-3.0) 4.0 5.0

        let expected = RTCTuple.point 2.0 1.0 7.0

        let result = Matrix.tmult transform p

        it "transform*p = point(2,1,7)" $
         RTCTuple.equal result expected  `shouldBe` True


    describe "Multiplying by the inverse of a translation matrix" $ do
        let transform = Transform.translation 5.0 (-3.0) 2.0

        let inv = Matrix.inverse transform

        let p = RTCTuple.point (-3.0) 4.0 5.0

        let expected = RTCTuple.point (-8.0) 7.0 3.0

        let result = Matrix.tmult inv p

        it "inv*p = point(-8,7,3)" $
         RTCTuple.equal result expected  `shouldBe` True

    describe "Translation does not affect vectors" $ do
        let transform = Transform.translation 5.0 (-3.0) 2.0


        let v = RTCTuple.vector (-3.0) 4.0 5.0


        let result = Matrix.tmult transform v
        it "transform*v=v" $
         RTCTuple.equal result v  `shouldBe` True

    describe "A scaling matrix applied to a point" $ do
        let transform = Transform.scaling 2.0 3.0 4.0

        let p = RTCTuple.point (-4.0) 6.0 8.0

        let expected = RTCTuple.point (-8.0) 18.0 32.0 

        let result = Matrix.tmult transform p

        it "transform*p = point(2,1,7)" $
         RTCTuple.equal result expected  `shouldBe` True

    describe "A scaling matrix applied to a vector" $ do
        let transform = Transform.scaling 2.0 3.0 4.0

        let p = RTCTuple.vector (-4.0) 6.0 8.0

        let expected = RTCTuple.vector (-8.0) 18.0 32.0 

        let result = Matrix.tmult transform p

        it "transform*p = point(2,1,7)" $
         RTCTuple.equal result expected  `shouldBe` True

    describe "Multiplying by the inverse of a scaling matrix" $ do
        let transform = Transform.scaling 2.0 3.0 4.0

        let inv = Matrix.inverse transform

        let v = RTCTuple.vector (-4.0) 6.0 8.0

        let expected = RTCTuple.vector (-2.0) 2.0 2.0

        let result = Matrix.tmult inv v

        it "inv*v = vector(-2,2,2)" $
         RTCTuple.equal result expected  `shouldBe` True

    describe "Reflection is scaling by a negative value" $ do
        let transform = Transform.scaling (-1.0) 1.0 1.0 

        let p = RTCTuple.point 2.0 3.0 4.0 

        let expected = RTCTuple.point (-2.0) 3.0 4.0 

        let result = Matrix.tmult transform p

        it "transform*p = point(2,1,7)" $
         RTCTuple.equal result expected  `shouldBe` True
