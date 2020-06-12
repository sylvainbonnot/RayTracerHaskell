module SphereSpec where

import Test.Hspec
import Matrix
import RTCTuple
import Data.Array
import Transform
import Intersection
import Material
import Ray (build, position, translate, scale) 
import Sphere 

spec :: Spec
spec = describe "Sphere" $ do

    describe "A ray intersects a sphere at two points" $ do

        let r = Ray.build (RTCTuple.point 0.0 0.0 (-5.0)) (RTCTuple.vector 0.0 0.0 1.0 )
        let s = sphere {sphereId = 0, transform = Matrix.identity}

        let xs = Sphere.intersect s r

        it "xs.count = 2" $
         length xs ==2  `shouldBe` True

        it "t xs[0]=4.0" $
         RTCTuple.approxEq (t (xs !! 0)) 4.0  `shouldBe` True

        it "t xs[1]=6.0" $
         RTCTuple.approxEq (t (xs !! 1)) 6.0  `shouldBe` True

    describe "A ray intersects a sphere at a tangent" $ do

        let r = Ray.build (RTCTuple.point 0.0 1.0 (-5.0)) (RTCTuple.vector 0.0 0.0 1.0 )
        let s = sphere {sphereId = 1, transform = Matrix.identity}

        let xs = Sphere.intersect s r

        it "xs.count = 2" $
         length xs ==2  `shouldBe` True

        it "t xs[0]=5.0" $
         RTCTuple.approxEq (t (xs !! 0)) 5.0  `shouldBe` True

        it "t xs[1]=5.0" $
         RTCTuple.approxEq (t (xs !! 1)) 5.0  `shouldBe` True


    describe "A ray misses a sphere" $ do

        let r = Ray.build (RTCTuple.point 0.0 2.0 (-5.0)) (RTCTuple.vector 0.0 0.0 1.0 )
        let s = sphere {sphereId = 2, transform = Matrix.identity}

        let xs = Sphere.intersect s r

        it "xs.count = 0" $
         length xs ==0  `shouldBe` True

    describe "A ray originates inside a sphere" $ do

        let r = Ray.build (RTCTuple.point 0.0 0.0 0.0) (RTCTuple.vector 0.0 0.0 1.0 )
        let s = sphere {sphereId = 3, transform = Matrix.identity}

        let xs = Sphere.intersect s r

        it "xs.count = 2" $
         length xs ==2  `shouldBe` True

        it "t xs[0]=-1.0" $
         RTCTuple.approxEq (t (xs !! 0)) (-1.0)  `shouldBe` True

        it "t xs[1]=1.0" $
         RTCTuple.approxEq (t (xs !! 1)) 1.0  `shouldBe` True



    describe "A sphere is behind a ray" $ do

        let r = Ray.build (RTCTuple.point 0.0 0.0 5.0) (RTCTuple.vector 0.0 0.0 1.0 )
        let s = sphere {sphereId = 4, transform = Matrix.identity}

        let xs = Sphere.intersect s r

        it "xs.count = 2" $
         length xs ==2  `shouldBe` True

        it "t xs[0]=-6.0" $
         RTCTuple.approxEq (t (xs !! 0)) (-6.0)  `shouldBe` True

        it "t xs[1]=-4.0" $
         RTCTuple.approxEq (t (xs !! 1)) (-4.0)  `shouldBe` True

    describe "Intersect sets the object on the intersection" $ do

        let r = Ray.build (RTCTuple.point 0.0 0.0 (-5.0)) (RTCTuple.vector 0.0 0.0 1.0 )
        let s = sphere {sphereId = 5, transform = Matrix.identity}

        let xs = Sphere.intersect s r

        it "xs.count = 2" $
         length xs ==2  `shouldBe` True

        it "object xs[0]=s" $
          (object (xs !! 0)) ==(Object s)  `shouldBe` True

        it "object xs[1]=s" $
         (object (xs !! 1)) ==(Object s)  `shouldBe` True

    describe "A sphere's default transformation" $ do

        let s = sphere {sphereId = 6, transform = Matrix.identity}


        it "s.transform = identity_matrix" $
         Matrix.equal (transform s) (Matrix.identity)  `shouldBe` True

    describe "Changing a sphere's transformation" $ do

        let s = sphere {sphereId = 7, transform = Matrix.identity}
        let t = Transform.translation 2.0 3.0 4.0 
        let s2= Sphere.setTransform s t 

        it "s.transform = t" $
         Matrix.equal (transform s2) t  `shouldBe` True

    describe "Intersecting a scaled sphere with a ray" $ do

        let r = Ray.build (RTCTuple.point 0.0 0.0 (-5.0)) (RTCTuple.vector 0.0 0.0 1.0 )
        let s = sphere {sphereId = 8, transform = Matrix.identity}
        let m = Transform.scaling 2.0 2.0 2.0 
        let s2 = Sphere.setTransform s m

        let xs = Sphere.intersect s2 r

        it "xs.count = 2" $
         length xs ==2  `shouldBe` True

        it "object xs[0]=s" $
          RTCTuple.approxEq (t (xs !! 0)) 3.0  `shouldBe` True

        it "object xs[1]=s" $
         RTCTuple.approxEq (t (xs !! 1)) 7.0  `shouldBe` True

    describe "Intersecting a translated sphere with a ray" $ do

        let r = Ray.build (RTCTuple.point 0.0 0.0 (-5.0)) (RTCTuple.vector 0.0 0.0 1.0 )
        let s = sphere {sphereId = 9, transform = Matrix.identity}
        let m = Transform.translation 5.0 0.0 0.0 
        let s3 = Sphere.setTransform s m

        let xs = Sphere.intersect s3 r

        it "xs.count = 0" $
         length xs ==0  `shouldBe` True

    describe "The normal on a sphere at a point on the x axis" $ do

        let p = RTCTuple.point  1.0 0.0 0.0 
        let s = sphere {sphereId = 10, transform = Matrix.identity}
        let n = RTCTuple.vector 1.0 0.0 0.0 
        let n2 = Sphere.normalAt s p


        it "n= vector(1,0,0)" $
         RTCTuple.equal n n2  `shouldBe` True

    describe "The normal on a sphere at a point on the y axis" $ do

        let p = RTCTuple.point  0.0 1.0 0.0 
        let s = sphere {sphereId = 10, transform = Matrix.identity}
        let n = RTCTuple.vector 0.0 1.0 0.0 
        let n2 = Sphere.normalAt s p


        it "n= vector(0,1,0)" $
         RTCTuple.equal n n2  `shouldBe` True

    describe "The normal on a sphere at a point on the z axis" $ do

        let p = RTCTuple.point  0.0 0.0 1.0 
        let s = sphere {sphereId = 10, transform = Matrix.identity}
        let n = RTCTuple.vector 0.0 0.0 1.0 
        let n2 = Sphere.normalAt s p


        it "n= vector(0,0,1)" $
         RTCTuple.equal n n2  `shouldBe` True

    describe "The normal on a sphere at a nonaxial point" $ do

        let p = RTCTuple.point  ((sqrt 3)/3.0)  ((sqrt 3)/3.0) ((sqrt 3)/3.0) 
        let s = sphere {sphereId = 10, transform = Matrix.identity}
        let n = RTCTuple.vector ((sqrt 3)/3.0)  ((sqrt 3)/3.0) ((sqrt 3)/3.0)
        let n2 = Sphere.normalAt s p


        it "n= vector(sqrt(3)/3, sqrt(3)/3, sqrt(3)/3)" $
         RTCTuple.equal n n2  `shouldBe` True


    describe "The normal is a normalized vector" $ do

        let p = RTCTuple.point  ((sqrt 3)/3.0)  ((sqrt 3)/3.0) ((sqrt 3)/3.0) 
        let s = sphere {sphereId = 10, transform = Matrix.identity}
        let n = Sphere.normalAt s p


        it "n= normalize(n)" $
         RTCTuple.equal n (norm n)  `shouldBe` True

    describe "A sphere has a default material" $ do

        let s = sphere 
        let m = material s

        it "m = defaultMaterial" $
         m == Material.defaultMaterial  `shouldBe` True

    describe "A sphere may be assigned a material" $ do

        let s = sphere 
        let m = defaultMaterial {ambient = 1.0}
        let newS= s {material = m}

        it "material s = m" $
         m == material newS  `shouldBe` True
        