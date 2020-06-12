module RTCTupleSpec where

import Test.Hspec

import RTCTuple

spec :: Spec
spec = do
  describe "Tuples components" $ do
    describe "A tuple with w == 1.0 is a point" $ do
      let p = build (4.3 :: Double) ((-4.2) :: Double) (3.1 :: Double) (1.0 :: Double)
      it "x == 4.3" $
        x p `shouldBe` (4.3 :: Double)
      it "y == -4.2" $
        y p `shouldBe` (-4.2)
      it "z == 3.1" $
        z p `shouldBe` 3.1
      it "w == 1.0" $
        w p `shouldBe` 1.0
      it "is a point" $
        ispoint p `shouldBe` True
      it "is not a vector" $
        isvector p `shouldBe` False

    describe "A tuple with w == 0.0 is a vector" $ do
      let v = build 4.3 (-4.2) 3.1 0.0
      it "x == 4.3" $
        x v `shouldBe` 4.3
      it "y == -4.2" $
        y v `shouldBe` (-4.2)
      it "z == 3.1" $
        z v `shouldBe` 3.1
      it "w == 0.0" $
        w v `shouldBe` 0.0
      it "is not a point" $
        ispoint v `shouldBe` False
      it "is a vector" $
        isvector v `shouldBe` True

    describe "'point' describes tuples with w=1" $ do
      let p = point 4 (-4) 3
      it "p = tuple(4, -4, 3, 1)" $
        p `shouldBe` RTCTuple 4 (-4) 3 1

    describe "'vector' describes tuples with w=0" $ do
      let v = vector 4 (-4) 3
      it "v = tuple(4, -4, 3, 0)" $
        v `shouldBe` RTCTuple 4 (-4) 3 0

  describe "Tuple Arithmetics" $ do
    describe "RTCTuple addition" $ do
      let a1  = build (3::Double) (-2::Double) (5::Double) (1::Double)
      let a2  = build ((-2)::Double) (3::Double) (1::Double) (0::Double)
      it "a1 + a2 = (1, 1, 6, 1)" $
        add a1  a2 `shouldBe` build 1 1 6 1

    describe "Subtracting two points" $ do
      let p1 = point 3 2 1
      let p2 = point 5 6 7
      it "p1 - p2 == vector (-2, -4, -6)" $
        RTCTuple.subtract p1  p2 `shouldBe` vector (-2) (-4) (-6)

    describe "Subtracting a vector from a point" $ do
      let p = point 3 2 1
      let v = vector 5 6 7
      it "p - v == point(-2, -4, -6)" $
        RTCTuple.subtract p  v `shouldBe` point (-2) (-4) (-6)

    describe "Subtracting two vectors" $ do
      let v1 = vector 3 2 1
      let v2 = vector 5 6 7
      it "v1 - v2 == vector(-2, -4, -6)" $
        RTCTuple.subtract v1  v2 `shouldBe` vector (-2) (-4) (-6)

    describe "Subtracting a vector from the zero vector" $ do
      let zero = vector 0 0 0
      let v = vector 1 (-2) 3
      it "zero - v = vector(-1, 2, -3)" $
        RTCTuple.subtract zero  v `shouldBe` vector (-1) 2 (-3)

    describe "Negating a tuple" $ do
      let a = RTCTuple 1 (-2) 3 (-4)
      it "-a = tuple (-1, 2, -3, 4)" $
        neg a `shouldBe` RTCTuple (-1) 2 (-3) 4

    describe "Multiplying a tuple by a scalar" $ do
      let a = RTCTuple 1 (-2) 3 (-4)
      it "mults a 3.5  = tuple(3.5, -7, 10.5, -14)" $
        mults a 3.5  `shouldBe` RTCTuple 3.5 (-7) 10.5 (-14)
    

    describe "Multiplying a tuple by a fraction" $ do
      let a = RTCTuple 1 (-2) 3 (-4)
      it "mults a  0.5 = tuple(0.5, -1, 1.5, -2)" $
        mults a  0.5 `shouldBe` RTCTuple 0.5 (-1) 1.5 (-2)

    describe "Dividing a tuple by a scalar" $ do
      let a = RTCTuple 1 (-2) 3 (-4)
      it "divs a  2 =  tuple(0.5, -1, 1.5, -2)" $
        divs a  2 `shouldBe` RTCTuple 0.5 (-1) 1.5 (-2)

  describe "Magnitude" $ do
    describe "Magnitude of vector(1, 0, 0)" $ do
      let v = vector 1 0 0
      it "mag v = 1" $
        mag v `shouldBe` 1.0

    describe "Magnitude of vector(0, 1, 0)" $ do
      let v = vector 0 1 0
      it "mag v = 1" $
        mag v `shouldBe` 1.0

    describe "Magnitude of vector(0, 0, 1)" $ do
      let v = vector 0 0 1
      it "mag v = 1" $
        mag v `shouldBe` 1.0

    describe "Magnitude of vector(1, 2, 3)" $ do
      let v = vector 1 2 3
      it "mag v = √14" $
        mag v `shouldBe` sqrt 14

    describe "Magnitude of vector(-1, -2, -3)" $ do
      let v = vector (-1) (-2) (-3)
      it "mag v = √14" $
        mag v `shouldBe` sqrt 14

  describe "Vector normalization" $ do

    describe "Normalizing vector(4, 0, 0) gives (1, 0, 0)" $ do
      let v = vector 4 0 0
      it "norm v = vector(1, 0, 0)" $
        norm v `shouldBe` vector 1 0 0
    describe "Normalizing vector(1, 2, 3)" $ do
      let v = vector 1 2 3
      it "norm v = ~= vector(0.26726, 0.53452, 0.80178)" $
        norm v `shouldBe` vector 0.2672612419124244 0.5345224838248488 0.8017837257372732
    describe "The magnitude of a normalized vector" $ do
      let v = vector 1 2 3
      let n = norm v
      it "magnitude(norm) = 1" $
        mag n `shouldBe` 1

  describe "Dot Product" $ do
    describe "The dot product of two tuples" $ do
      let a = vector 1 2 3
      let b = vector 2 3 4
      it "dot(a, b) = 20" $
        (a `dot` b) `shouldBe` 20

  describe "Cross Product" $ do
    describe "Cross product of two vectors" $ do
      let a = vector 1 2 3
      let b = vector 2 3 4
      it "a ⨯ b = vector(-1, 2, -1)" $
        cross a  b `shouldBe` vector (-1) 2 (-1)
      it "b ⨯ a = vector(1, -2, 1)" $
        cross b  a `shouldBe` vector 1 (-2) 1


  describe "Reflecting a vector approaching at 45 degrees" $ do
    
      let v = vector 1.0 (-1.0) 0.0
      let n = vector 0.0 1.0 0.0
      let expected = vector 1.0 1.0 0.0
      let r = reflect v n
      it "reflect(v,n) = expected" $
        RTCTuple.equal r expected `shouldBe` True

  describe "Reflecting a vector off a slanted surface" $ do
    
      let v = vector 0.0 (-1.0) 0.0
      let a = (sqrt 2.0)/2.0
      let n = vector a a 0.0
      let expected = vector 1.0 0.0 0.0
      let r = reflect v n
      it "reflect(v,n) = expected" $
        RTCTuple.equal r expected `shouldBe` True