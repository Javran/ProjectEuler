{-# LANGUAGE
    ScopedTypeVariables
  #-}
module ProjectEuler.SolCommonSpec where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Poly
import Petbox

import ProjectEuler.SolCommon

spec :: Spec
spec =
  describe "pickInOrder'" $ do
    specify "example" $
      pickInOrder' "abcd1"
        `shouldBe`
          [ ('a', "abcd1")
          , ('b', "bcd1")
          , ('c', "cd1")
          , ('d', "d1")
          , ('1', "1")
          ]
    specify "correctness" $
      property $
        \(xs :: [A]) ->
          pickInOrder' xs === fmap (\(u,v) -> (u,u:v)) (pickInOrder xs)
