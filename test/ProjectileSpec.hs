module ProjectileSpec where

import Test.Hspec

import Projectile
import RTCTuple

{-
main :: IO ()
main = do
  tree <- testSpec "hspec tests" testSuite
  defaultMain tree
-}

spec :: Spec
spec = do
  describe "Projectile" $ do
    describe "when only has gravity vector" $ do
      let env = Environment{ gravity = vector 0 (-9.8) 0, wind = vector 0 0 0 }
      let p = Projectile { position = point 0 9.8 0, velocity = vector 0 0 0 }

      it "After 0 second without velocity, position shouldn't change, velocity has increased" $ do
        let p1 = tick env p
        position p1 `shouldBe` point 0 9.8 0
        velocity p1 `shouldBe` vector 0 (-9.8) 0

      it "After 1 second without velocity, position is ground level, velocity has doubled" $ do
        let p1 = tick env (tick env p)
        position p1 `shouldBe` point 0 0 0
        velocity p1 `shouldBe` vector 0 (-19.6) 0

    describe "when only has wind vector" $ do
      let env = Environment{ gravity = vector 0 0 0, wind = vector 0 (-9.8) 0 }
      let p = Projectile { position = point 0 9.8 0, velocity = vector 0 0 0 }

      it "After 0 second without velocity, position shouldn't change, velocity has increased" $ do
        let p1 = tick env p
        position p1 `shouldBe` point 0 9.8 0
        velocity p1 `shouldBe` vector 0 (-9.8) 0

      it "After 1 second without velocity, position is ground level, velocity has doubled" $ do
        let p1 = tick env (tick env p)
        position p1 `shouldBe` point 0 0 0
        velocity p1 `shouldBe` vector 0 (-19.6) 0

    describe "when only has gravity and wind" $ do
      let env = Environment{ gravity = vector 0 (-9.8) 0, wind = vector 0 0 1 }
      let p = Projectile { position = point 0 9.8 1, velocity = vector 0 0 0 }

      it "After 0 second without velocity, position shouldn't change, velocity has increased" $ do
        let p1 = tick env p
        position p1 `shouldBe` point 0 9.8 1
        velocity p1 `shouldBe` vector 0 (-9.8) 1

      it "After 1 second without velocity, position 0 0 2 and velocity 0 -19.6 2" $ do
        let p1 = tick env (tick env p)
        position p1 `shouldBe` point 0 0 2
        velocity p1 `shouldBe` vector 0 (-19.6) 2

    describe "when has gravity and wind and the projectile has a certain velocity" $ do
      let env = Environment{ gravity = vector 0 (-9.8) 0, wind = vector 0 0 1 }
      let p = Projectile { position = point 0 9.8 1, velocity = vector 1 0 1 }

      it "After 0 second position should have changed and velocity has increased" $ do
        let p1 = tick env p
        position p1 `shouldBe` point 1 9.8 2
        velocity p1 `shouldBe` vector 1 (-9.8) 2

      it "After 1 second position is 2 0 4, velocity is 1 -19.6 3" $ do
        let p1 = tick env (tick env p)
        position p1 `shouldBe` point 2 0 4
        velocity p1 `shouldBe` vector 1 (-19.6) 3
