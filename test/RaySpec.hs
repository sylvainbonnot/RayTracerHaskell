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
