{-# LANGUAGE
    TypeApplications
  #-}
module ProjectEuler.SolCommonSpec where

import Test.Hspec

import ProjectEuler.SolCommon

spec :: Spec
spec = do
  describe "factorials" $
    specify "first few elements" $
      take 10 factorials
        `shouldBe` [1,1,2,6,24,120,720,5040,40320,362880]

  describe "intToDigits" $
    specify "examples" $ do
      intToDigits @Integer 1234 `shouldBe` [1,2,3,4]
      intToDigits @Int 8 `shouldBe` [8]
      intToDigits @Integer 43212345566 `shouldBe` [4,3,2,1,2,3,4,5,5,6,6]
