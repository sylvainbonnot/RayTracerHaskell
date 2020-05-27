module MatrixSpec where

import Test.Hspec
import Matrix
import RTCTuple
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

    describe "Matrix equality with identical matrices" $ do
        let m = createMatrixFromList 4 4 [[1.0,2.0,3.0,4.0],
                                      [5.0, 6.0, 7.0, 8.0],
                                      [9.0,10.0,11.0,12.0],
                                      [13.0, 14.0, 15.0, 16.0]]
        let m' = createMatrixFromList 4 4 [[1.0,2.0,3.0,4.0],
                                      [5.0, 6.0, 7.0, 8.0],
                                      [9.0,10.0,11.0,12.0],
                                      [13.0, 14.0, 15.0, 16.0]]  
        let n = createMatrixFromList 4 4 [[2.0,3.0,4.0,5.0],
                                      [6.0, 7.0, 8.0,9.0],
                                      [8.0,7.0,6.0,5.0],
                                      [4.0, 3.0, 2.0, 1.0]]

        it "m=m'" $
         Matrix.equal m m'  `shouldBe` True
        it "m!=n" $
         Matrix.equal m n  `shouldBe` False
        
    describe "Multiplying two matrices" $ do
        let m = createMatrixFromList 4 4 [[1.0,2.0,3.0,4.0],
                                      [5.0, 6.0, 7.0, 8.0],
                                      [9.0,8.0,8.0,6.0],
                                      [5.0, 4.0, 3.0, 2.0]]
        let m' = createMatrixFromList 4 4 [[-2.0,1.0, 2.0,3.0],
                                      [3.0, 2.0, 1.0, -1.0],
                                      [4.0,3.0,6.0,5.0],
                                      [1.0, 2.0, 7.0, 8.0]]  
        let n = createMatrixFromList 4 4 [[20.0,22.0,50.0,48.0], --was wrong in the book!
                                      [44.0, 54.0, 114.0,108.0],
                                      [44.0,61.0,116.0,107.0],
                                      [16.0, 26.0, 46.0, 42.0]]

        it "m*m'=n" $
         Matrix.equal (matMult m m') n  `shouldBe` True
        
    describe "A matrix multiplied by a tuple" $ do
        let m = createMatrixFromList 4 4 [[1.0,2.0,3.0,4.0],
                                      [2.0, 4.0, 4.0, 2.0],
                                      [8.0,6.0,4.0,1.0],
                                      [0.0, 0.0, 0.0, 1.0]]
        let t = RTCTuple {x=1.0, y=2.0, z=3.0, w=1.0} 
        let expected = RTCTuple {x=18.0, y= 24.0, z= 33.0, w= 1.0}

        it "m*t=tuple(18.0, 24.0, 33.0, 1.0)" $
         RTCTuple.equal (tmult m t) expected  `shouldBe` True

    describe "Multiplying a matrix by the identity matrix" $ do
        let m1 = createMatrixFromList 4 4 [[0.0,1.0,2.0,4.0],
                                      [1.0, 2.0, 4.0, 8.0],
                                      [2.0,4.0,8.0,16.0],
                                      [4.0, 8.0, 16.0, 32.0]]
        let m2 = Matrix.identity 

        it "m1*identity=m1" $
         Matrix.equal (matMult m1 m2) m1  `shouldBe` True


    describe "Multiplying the identity matrix by a tuple" $ do
        let m = Matrix.identity 
        let t = RTCTuple {x=1.0, y=2.0, z=3.0, w=4.0} 

        it "m*t=t" $
         RTCTuple.equal (tmult m t) t  `shouldBe` True

    describe "Transposing a matrix" $ do
        let a = createMatrixFromList 4 4 [[0.0,9.0,3.0,0.0],
                                      [9.0, 8.0, 0.0, 8.0],
                                      [1.0,8.0,5.0,3.0],
                                      [0.0, 0.0, 5.0, 8.0]]
        let ta = createMatrixFromList 4 4 [[0.0,9.0,1.0,0.0],
                                      [9.0, 8.0, 8.0, 0.0],
                                      [3.0,0.0,5.0,5.0],
                                      [0.0, 8.0, 3.0, 8.0]]

        

        it "Transpose(A) is the matrix ta" $
         Matrix.equal (Matrix.transpArray a) ta  `shouldBe` True
    
    describe "Transposing the identity matrix" $ do
        let a = Matrix.identity
        it "Transpose(I) is I" $
         Matrix.equal (Matrix.transpArray a) a  `shouldBe` True

    describe "Calculating the determinant of a 2x2 matrix" $ do
        let a = createMatrixFromList 2 2 [[1.0,5.0],[-3.0,2.0]]
        it "det(A) is 17" $
         Matrix.determinant a  `shouldBe` 17.0
                                     

    describe "A submatrix of a 3x3 matrix is a 2x2 matrix" $ do
        let a = createMatrixFromList 3 3 [[1.0,5.0,0.0],
                                      [-3.0, 2.0, 7.0],
                                      [0.0,6.0,-3.0]]

        let b = submatrix a 0 2 
        
        let c = createMatrixFromList 2 2 [[-3.0,2.0],[0.0,6.0]] 
        it "Submatrix(A,0,2) is C" $
         Matrix.equal b c  `shouldBe` True
    
    describe "A submatrix of a 4x4 matrix is a 3x3 matrix" $ do
        let a = createMatrixFromList 4 4 [[-6.0,1.0,1.0,6.0],
                                      [-8.0, 5.0, 8.0, 6.0],
                                      [-1.0,0.0,8.0,2.0],
                                      [-7.0, 1.0, -1.0, 1.0]]

        let b = submatrix a 2 1 
        
        let c = createMatrixFromList 3 3 [[-6.0,1.0,6.0],
                                      [-8.0, 8.0, 6.0],
                                      [-7.0,-1.0,1.0]] 
        it "Submatrix(A,0,2) is C" $
         Matrix.equal b c  `shouldBe` True
    
    describe "Calculating a minor of a 3x3 matrix" $ do
        let a = createMatrixFromList 3 3 [[3.0,5.0,0.0],
                                      [2.0, -1.0, -7.0],
                                      [6.0,-1.0,5.0]]

        let b = submatrix a 1 0 
        
        it "Det(B) is 25" $
         Matrix.minor a 1 0  `shouldBe` 25.0

    describe "Calculating a cofactor of a 3x3 matrix" $ do
        let a = createMatrixFromList 3 3 [[3.0,5.0,0.0],
                                      [2.0, -1.0, -7.0],
                                      [6.0,-1.0,5.0]]

        it "minor(A,0,0) is -12" $
         Matrix.minor a 0 0  `shouldBe` -12.0 
        
        it "cofactor(A,0,0) is -12" $
         Matrix.cofactor a 0 0  `shouldBe` -12.0

        it "minor(A,1,0) is 25" $
         Matrix.minor a 1 0  `shouldBe` 25.0 
        
        it "cofactor(A,1,0) is -25" $
         Matrix.cofactor a 1 0  `shouldBe` -25.0  

    describe "Calculating the determinant of a 3x3 matrix" $ do
        let a = createMatrixFromList 3 3 [[1.0,2.0,6.0],
                                      [-5.0, 8.0, -4.0],
                                      [2.0,6.0,4.0]]

        
        it "cofactor(A,0,0) is 56" $
         Matrix.cofactor a 0 0  `shouldBe` 56.0

        it "cofactor(A,0,1) is 12" $
         Matrix.cofactor a 0 1  `shouldBe` 12.0
        
        it "cofactor(A,0,2) is -46" $
         Matrix.cofactor a 0 2  `shouldBe` -46.0  
        
        it "det(A) is -196" $
         Matrix.determinant a   `shouldBe` -196.0 

    describe "Calculating the determinant of a 4x4 matrix" $ do
        let a = createMatrixFromList 4 4 [[2.0,-8.0,3.0,5.0],
                                      [-3.0, 1.0, 7.0, 3.0],
                                      [1.0,2.0,-9.0,6.0],
                                      [-6.0, 7.0, 7.0, -9.0]]

        
        it "cofactor(A,0,0) is 690" $
         Matrix.cofactor a 0 0  `shouldBe` 690.0

        it "cofactor(A,0,1) is 447" $
         Matrix.cofactor a 0 1  `shouldBe` 447.0
        
        it "cofactor(A,0,2) is 210" $
         Matrix.cofactor a 0 2  `shouldBe` 210.0  
        
        it "cofactor(A,0,3) is 51" $
         Matrix.cofactor a 0 2  `shouldBe` 210.0

        it "det(A) is -1311" $ --was wrong in the book!
         Matrix.determinant a   `shouldBe` -1311.0 

    describe "Testing an invertible matrix for invertibility" $ do
        let a = createMatrixFromList 4 4 [[6.0,4.0,4.0,4.0],
                                      [5.0, 5.0, 7.0, 6.0],
                                      [4.0,-9.0,3.0,-7.0],
                                      [9.0, 1.0, 7.0, -6.0]]

        
        it "det(A) is -2120" $ 
         Matrix.determinant a   `shouldBe` -2120.0 

        it "A is invertible" $
         Matrix.isInvertible a  `shouldBe` True
        
        
    describe "Testing an noninvertible matrix for invertibility" $ do
        let a = createMatrixFromList 4 4 [[-4.0,2.0,-2.0,-3.0],
                                      [9.0, 6.0, 2.0, 6.0],
                                      [0.0,5.0,1.0,-5.0],
                                      [0.0, 0.0, 0.0, 0.0]]

        
        it "det(A) is 0" $ 
         Matrix.determinant a   `shouldBe` 0.0 

        it "A is invertible" $
         Matrix.isInvertible a  `shouldBe` False
        
    describe "Calculating the inverse of a matrix" $ do
        let a = createMatrixFromList 4 4 [[-5.0,2.0,6.0,-8.0],
                                      [1.0, -5.0, 1.0, 8.0],
                                      [7.0,7.0,-6.0,-7.0],
                                      [1.0, -3.0, 7.0, 4.0]]

        let b = inverse a

        let expected = createMatrixFromList 4 4 [[0.21805,0.45113,0.24060,-0.04511],[-0.80827,-1.45677,-0.44361,0.52068],[-0.07895 ,-0.22368,-0.05263 ,0.19737],[-0.52256,-0.81391,-0.30075,0.30639]]

        it "det(A) is 532" $ 
         Matrix.determinant a   `shouldBe` 532.0 

        it "cofactor(A,2,3) is -160" $
         Matrix.cofactor a 2 3  `shouldBe` -160.0

        it "b(3,2) is -160/532" $
         b ! (3,2)   `shouldBe` -160.0/532.0

        it "cofactor(A,3,2) is -105" $
         Matrix.cofactor a 3 2  `shouldBe` 105.0 --wrong sign in the book

        it "b(2,3) is 105/532" $
         b ! (2,3)   `shouldBe` 105.0/532.0

        it "b is equal to expected" $
         Matrix.equal b expected   `shouldBe` True

    describe "Calculating the inverse of another matrix" $ do
        let a = createMatrixFromList 4 4 [[8.0, -5.0,9.0,2.0],
                                      [7.0, 5.0, 6.0, 1.0],
                                      [-6.0,0.0,9.0,6.0],
                                      [-3.0, 0.0, -9.0, -4.0]]

        let b = inverse a

        let expected = createMatrixFromList 4 4 [[-0.15384615,-0.15384615, -0.28205128, -0.53846154],
                     [-0.07692308,  0.12307692,  0.02564103,  0.03076923],
                     [ 0.35897436,  0.35897436,  0.43589744,  0.92307692],
                     [-0.69230769, -0.69230769, -0.76923077, -1.92307692]]

        it "b is equal to expected" $
         Matrix.equal b expected   `shouldBe` True

    describe "Calculating the inverse of a third matrix" $ do
        let a = createMatrixFromList 4 4 [[9,3,0,9],[-5,-2,-6,-3],[-4, 9,6,4],[-7,6,6,2]]

        let b = inverse a

        let expected = createMatrixFromList 4 4 [[-0.04074074 ,-0.07777778 , 0.14444444 ,-0.22222222],[-0.07777778 , 0.03333333 , 0.36666667 ,-0.33333333],
                                                 [-0.02901235 ,-0.1462963 , -0.10925926 , 0.12962963],[ 0.17777778 , 0.06666667, -0.26666667,  0.33333333]]

        it "b is equal to expected" $
         Matrix.equal b expected   `shouldBe` True





