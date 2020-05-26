module MatrixSpec where

import Test.Hspec
import Matrix
import Data.Array

spec :: Spec
spec = describe "Matrix" $ do

    describe "Constructing and inspecting a 4x4 Matrix" $ do
        let m = createMatrixFromList 4 4 [[1,2,3,4],
                                      [5.5, 6.5, 7.5, 8.5],
                                      [9,10,11,12],
                                      [13.5, 14.5, 15.5, 16.5]]
        it "M[0,0] = 1" $
         m ! (0,0)  `shouldBe` 1
        it "M[0,3] = 1" $
         m ! (0,3)  `shouldBe` 4
        it "M[1,0] = 1" $
         m ! (1,0)  `shouldBe` 5.5
        it "M[1,2] = 1" $
         m ! (1,2)  `shouldBe` 7.5
        it "M[2,2] = 1" $
         m ! (2,2)  `shouldBe` 11
        it "M[3,0] = 1" $
         m ! (3,0)  `shouldBe` 13.5

        it "M[3,2] = 1" $
         m ! (3,2)  `shouldBe` 15.5

    describe "A 2x2 matrix ought to be representable" $ do
        let m = createMatrixFromList 2 2 [[-3, 5],
                                      [1, -2]]
        it "M[0,0] = -3" $
         m ! (0,0)  `shouldBe` -3
        it "M[0,1] = 5" $
         m ! (0,1)  `shouldBe` 5
        it "M[1,0] = 1" $
         m ! (1,0)  `shouldBe` 1
        it "M[1,1] = -2" $
         m ! (1,1)  `shouldBe` -2
        
    describe "A 3x3 matrix ought to be representable" $ do
        let m = createMatrixFromList 3 3 [[-3, 5,0],
                                      [1, -2,-7],
                                      [0,1,1]]
        it "M[0,0] = -3" $
         m ! (0,0)  `shouldBe` -3
        it "M[1,1] = -2" $
         m ! (1,1)  `shouldBe` -2
        it "M[2,2] = 1" $
         m ! (2,2)  `shouldBe` 1
        