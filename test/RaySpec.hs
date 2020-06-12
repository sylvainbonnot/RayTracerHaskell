module RaySpec where

import Test.Hspec
import Matrix
import RTCTuple
import Data.Array
import Transform
import Ray

spec :: Spec
spec = describe "Ray" $ do

    describe "Creating and querying a Ray" $ do

        let or = RTCTuple.point 1.0 2.0 3.0 
        let dir = RTCTuple.vector 4.0 5.0 6.0

        let r = Ray.build or dir

        it "r.origin = origin" $
         RTCTuple.equal or (origin r)  `shouldBe` True

        it "r.direction = direction" $
         RTCTuple.equal dir (direction r)  `shouldBe` True

    describe "Computing a point from a distance" $ do

        let or = RTCTuple.point 2.0 3.0 4.0 
        let dir = RTCTuple.vector 1.0 0.0 0.0

        let r = Ray.build or dir

        it "position(r, 0) is point(2,3,4)" $
         RTCTuple.equal (position r 0.0) (RTCTuple.point 2.0 3.0 4.0)  `shouldBe` True

        it "position(r, 1) is point(3,3,4)" $
         RTCTuple.equal (position r 1.0) (RTCTuple.point 3.0 3.0 4.0)  `shouldBe` True

        it "position(r, -1) is point(1,3,4)" $
         RTCTuple.equal (position r (-1.0)) (RTCTuple.point 1.0 3.0 4.0)  `shouldBe` True

        it "position(r, 2.5) is point(4.5,3,4)" $
         RTCTuple.equal (position r 2.5) (RTCTuple.point 4.5 3.0 4.0)  `shouldBe` True

    describe "Translating a ray" $ do

        let or = RTCTuple.point 1.0 2.0 3.0 
        let dir = RTCTuple.vector 0.0 1.0 0.0

        let r = Ray.build or dir

        let trans = Transform.translation 3.0 4.0 5.0 

        let new_ray = Ray.translate r trans

        it "origin(new_ray) is point(4,6,8)" $
         RTCTuple.equal (origin new_ray) (RTCTuple.point 4.0 6.0 8.0)  `shouldBe` True

        it "direction(new_ray) is vector(0,1,0)" $
         RTCTuple.equal (direction new_ray) (RTCTuple.vector 0.0 1.0 0.0)  `shouldBe` True

    describe "Scaling a ray" $ do

        let or = RTCTuple.point 1.0 2.0 3.0 
        let dir = RTCTuple.vector 0.0 1.0 0.0

        let r = Ray.build or dir

        let trans = Transform.scaling 2.0 3.0 4.0 

        let new_ray = Ray.scale r trans

        it "origin(new_ray) is point(2,6,12)" $
         RTCTuple.equal (origin new_ray) (RTCTuple.point 2.0 6.0 12.0)  `shouldBe` True

        it "direction(new_ray) is vector(0,3,0)" $
         RTCTuple.equal (direction new_ray) (RTCTuple.vector 0.0 3.0 0.0)  `shouldBe` True

