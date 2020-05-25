module ColorSpec where

import Test.Hspec
import Color

spec :: Spec
spec = describe "Color" $ do
    describe "Colors are (red, green, blue) tuples" $ do
        let c = Color (-0.5, 0.4, 1.7) 
        it "c.red == -0.5" $
            red c `shouldBe` (-0.5)
        it "c.green == 0.4" $
            green c `shouldBe` 0.4
        it "c.blue == 1.7" $
            blue c `shouldBe` 1.7

    describe "Color Operations" $ do
        describe "Adding colors" $ do
            let c1 = Color (0.9, 0.6, 0.75) 
            let c2 = Color (0.7, 0.1, 0.25) 
            it "c1 + c2 == Color (1.6, 0.7, 1.0)" $
                (c1 + c2) ~= Color (1.6, 0.7, 1.0)
        describe "Subtracting colors" $ do
            let c1 = Color (0.9, 0.6, 0.75) 
            let c2 = Color (0.7, 0.1, 0.25) 
            it "c1 - c2 == Color (0.2, 0.5, 0.5)" $
                (c1 - c2) ~= Color (0.2, 0.5, 0.5)
        describe "Multiplying a color by a scalar" $ do
            let c = Color (0.2, 0.3, 0.4) 
            it "mults c 2 == Color (0.4, 0.6, 0.8)" $
              mults c 2 ~= Color (0.4, 0.6, 0.8)
        describe "Multiplying colors" $ do
            let c1 = Color(1, 0.2, 0.4)
            let c2 = Color (0.9, 1, 0.1)
            it "mult c1  c2 == Color (0.9, 0.2, 0.04)" $
              mult c1  c2 ~= Color (0.9, 0.2, 0.04)




class ApproxEq a where
    (~=) :: a -> a -> Bool
    (/~=) :: a -> a -> Bool

instance ApproxEq Float where
    x ~= x' = abs (x-x') < epsilon
        where epsilon = 0.000001 -- good enough for Floats
    x /~= x' = not $ x ~= x'

instance ApproxEq Color where
    Color (r, g, b) ~= Color (r', g', b') = r ~= r' && g ~= g' && b ~= b'
    x /~= x' = not $ x ~= x'