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

    describe "Rotating a point around the x axis" $ do
        let half_quarter = Transform.rotation_x (pi/4.0)
        let full_quarter = Transform.rotation_x (pi/2.0)   

        let p = RTCTuple.point 0.0 1.0 0.0 

        let expected_half = RTCTuple.point 0.0 (0.5*sqrt 2) (0.5*sqrt 2) 
        let expected_full = RTCTuple.point 0.0 0.0 1.0 

        let result_half = Matrix.tmult half_quarter p
        let result_full = Matrix.tmult full_quarter p
        it "half_quarter*p = expected_half" $
         RTCTuple.equal result_half expected_half  `shouldBe` True

        it "full_quarter*p = expected_full" $
         RTCTuple.equal result_full expected_full  `shouldBe` True

    describe "The inverse of an x rotation rotates in the opposite direction" $ do
        let half_quarter = Transform.rotation_x (pi/4.0)
        let inv = Matrix.inverse half_quarter   

        let p = RTCTuple.point 0.0 1.0 0.0 

        let expected_inv = RTCTuple.point 0.0 (0.5*sqrt 2) (-0.5*sqrt 2) 
       

        let result_inv = Matrix.tmult inv p

        it "inv*p = expected_inv" $
         RTCTuple.equal result_inv expected_inv  `shouldBe` True

    describe "Rotating a point around the y axis" $ do
        let half_quarter = Transform.rotation_y (pi/4.0)
        let full_quarter = Transform.rotation_y (pi/2.0)   

        let p = RTCTuple.point 0.0 0.0 1.0 

        let expected_half = RTCTuple.point (0.5*sqrt 2) 0.0 (0.5*sqrt 2) 
        let expected_full = RTCTuple.point 1.0 0.0 0.0 

        let result_half = Matrix.tmult half_quarter p
        let result_full = Matrix.tmult full_quarter p
        it "half_quarter*p = expected_half" $
         RTCTuple.equal result_half expected_half  `shouldBe` True

        it "full_quarter*p = expected_full" $
         RTCTuple.equal result_full expected_full  `shouldBe` True


    describe "Rotating a point around the z axis" $ do
        let half_quarter = Transform.rotation_z (pi/4.0)
        let full_quarter = Transform.rotation_z (pi/2.0)   

        let p = RTCTuple.point 0.0 1.0 0.0 

        let expected_half = RTCTuple.point (-0.5*sqrt 2) (0.5*sqrt 2) 0.0 
        let expected_full = RTCTuple.point (-1.0) 0.0 0.0 

        let result_half = Matrix.tmult half_quarter p
        let result_full = Matrix.tmult full_quarter p
        it "half_quarter*p = expected_half" $
         RTCTuple.equal result_half expected_half  `shouldBe` True

        it "full_quarter*p = expected_full" $
         RTCTuple.equal result_full expected_full  `shouldBe` True    


    describe "A shearing transformation moves x in proportion to y" $ do
        let transform = Transform.shearing 1.0 0.0 0.0 0.0 0.0 0.0  
           

        let p = RTCTuple.point 2.0 3.0 4.0 

        let expected = RTCTuple.point 5.0 3.0 4.0
         

        let result = Matrix.tmult transform p
        
        it "transform*p = expected" $
         RTCTuple.equal result expected  `shouldBe` True

    describe "A shearing transformation moves x in proportion to z" $ do
        let transform = Transform.shearing 0.0 1.0 0.0 0.0 0.0 0.0  
           

        let p = RTCTuple.point 2.0 3.0 4.0 

        let expected = RTCTuple.point 6.0 3.0 4.0
         

        let result = Matrix.tmult transform p
        
        it "transform*p = expected" $
         RTCTuple.equal result expected  `shouldBe` True


    describe "A shearing transformation moves y in proportion to x" $ do
        let transform = Transform.shearing 0.0 0.0 1.0 0.0 0.0 0.0  
           

        let p = RTCTuple.point 2.0 3.0 4.0 

        let expected = RTCTuple.point 2.0 5.0 4.0
         

        let result = Matrix.tmult transform p
        
        it "transform*p = expected" $
         RTCTuple.equal result expected  `shouldBe` True

    describe "A shearing transformation moves y in proportion to z" $ do
        let transform = Transform.shearing 0.0 0.0 0.0 1.0 0.0 0.0  
           

        let p = RTCTuple.point 2.0 3.0 4.0 

        let expected = RTCTuple.point 2.0 7.0 4.0
         

        let result = Matrix.tmult transform p
        
        it "transform*p = expected" $
         RTCTuple.equal result expected  `shouldBe` True

    describe "A shearing transformation moves z in proportion to x" $ do
        let transform = Transform.shearing 0.0 0.0 0.0 0.0 1.0 0.0  
           

        let p = RTCTuple.point 2.0 3.0 4.0 

        let expected = RTCTuple.point 2.0 3.0 6.0
         

        let result = Matrix.tmult transform p
        
        it "transform*p = expected" $
         RTCTuple.equal result expected  `shouldBe` True

    describe "A shearing transformation moves z in proportion to y" $ do
        let transform = Transform.shearing 0.0 0.0 0.0 0.0 0.0 1.0  
           

        let p = RTCTuple.point 2.0 3.0 4.0 

        let expected = RTCTuple.point 2.0 3.0 7.0
         

        let result = Matrix.tmult transform p
        
        it "transform*p = expected" $
         RTCTuple.equal result expected  `shouldBe` True


    describe "A shearing transformation moves z in proportion to y" $ do
        let transform = Transform.shearing 0.0 0.0 0.0 0.0 0.0 1.0  
           

        let p = RTCTuple.point 1.0 0.0 1.0 
        let a = Transform.rotation_x (pi/2.0)
        let b = Transform.scaling 5.0 5.0 5.0 
        let c = Transform.translation 10.0 5.0 7.0 
        let p2 = Matrix.tmult a p 
        let p2_expected = RTCTuple.point 1.0 (-1.0) 0.0 
        let p3 =  Matrix.tmult b p2 
        let p3_expected = RTCTuple.point 5.0 (-5.0) 0.0 
        let p4 = Matrix.tmult c p3
        let p4_expected = RTCTuple.point 15.0 0.0 7.0 

        it "a*p = p2_expected" $
         RTCTuple.equal p2 p2_expected  `shouldBe` True

        it "b*p2 = p3_expected" $
         RTCTuple.equal p3 p3_expected  `shouldBe` True

        it "c*p3 = p4_expected" $
         RTCTuple.equal p4 p4_expected  `shouldBe` True

        
    describe "Chained transformations must be applied in reverse order" $ do
        let transform = Transform.shearing 0.0 0.0 0.0 0.0 0.0 1.0  
           

        let p = RTCTuple.point 1.0 0.0 1.0 
        let a = Transform.rotation_x (pi/2.0)
        let b = Transform.scaling 5.0 5.0 5.0 
        let c = Transform.translation 10.0 5.0 7.0 
        let t = Matrix.matMult c  (Matrix.matMult b  a) 
        let result_expected = RTCTuple.point 15.0 0.0 7.0 
        let result = Matrix.tmult t p
        it "a*p = p2_expected" $
         RTCTuple.equal result result_expected  `shouldBe` True

        