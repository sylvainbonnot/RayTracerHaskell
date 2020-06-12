module IntersectionSpec where

import Test.Hspec
import Matrix
import RTCTuple
import Data.Array
import Transform
import Ray
import Sphere

spec :: Spec
spec = describe "Intersection" $ do

    describe "An intersection encapsulates t and object" $ do

        let s = sphere {sphereId = 0, Sphere.transform = Matrix.identity}

        let inter = Intersection {t = 3.5, object = Object s}

        it "i.t = 3.5" $
         RTCTuple.approxEq (t inter) 3.5  `shouldBe` True

        it "i.object = s" $
         (object inter)==Object s  `shouldBe` True

    describe "Aggregating intersections" $ do

        let s = sphere {sphereId = 1, Sphere.transform = Matrix.identity}

        let i1 = Intersection {t = 1.0, object = Object s}

        let i2 = Intersection {t = 2.0, object = Object s}

        let xs = [i1, i2]::Intersections

        it "xs.count = 2" $
         length xs  `shouldBe` 2

        it "xs[0].t = 1.0" $
         RTCTuple.approxEq (t (xs !! 0)) 1.0   `shouldBe` True

        it "xs[1].t = 2.0" $
         RTCTuple.approxEq (t (xs !! 1)) 2.0   `shouldBe` True

    describe "The hit, when all intersections have positive t" $ do

        let s = sphere {sphereId = 2, Sphere.transform = Matrix.identity}

        let i1 = Intersection {t = 1.0, object = Object s}

        let i2 = Intersection {t = 2.0, object = Object s}

        let xs = [i1, i2]::Intersections

        it "hit(xs) = i1" $
         hit(xs)==Just i1  `shouldBe` True

    describe "The hit, when some intersections have negative t" $ do

        let s = sphere {sphereId = 3, Sphere.transform = Matrix.identity}

        let i1 = Intersection {t = -1.0, object = Object s}

        let i2 = Intersection {t = 1.0, object = Object s}

        let xs = [i2, i1]::Intersections

        it "hit(xs) = i1" $
         hit(xs)==Just i2  `shouldBe` True

    describe "The hit, when all intersections have negative t" $ do

        let s = sphere {sphereId = 4, Sphere.transform = Matrix.identity}

        let i1 = Intersection {t = -2.0, object = Object s}

        let i2 = Intersection {t = -1.0, object = Object s}

        let xs = [i2, i1]::Intersections

        it "hit(xs) = []" $
         hit(xs)==Nothing  `shouldBe` True


    describe "The hit is always the lowest nonnegative intersection" $ do

        let s = sphere {sphereId = 5, Sphere.transform = Matrix.identity}

        let i1 = Intersection {t = 5.0, object = Object s}

        let i2 = Intersection {t = 7.0, object = Object s}
        let i3 = Intersection {t = -3.0, object = Object s}
        let i4 = Intersection {t = 2.0, object = Object s}

        let xs = [i1, i2, i3, i4]::Intersections

        it "hit(xs) = i4" $
         hit(xs)==Just i4  `shouldBe` True