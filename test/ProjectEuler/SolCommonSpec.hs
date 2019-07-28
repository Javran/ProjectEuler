{-# LANGUAGE
    TypeApplications
  , BinaryLiterals
  , ScopedTypeVariables
  #-}
module ProjectEuler.SolCommonSpec where

import Data.List
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Poly

import ProjectEuler.SolCommon

spec :: Spec
spec = do
  describe "factorials" $
    specify "first few elements" $
      take 10 factorials
        `shouldBe` [1,1,2,6,24,120,720,5040,40320,362880]

  describe "intToDigits" $
    specify "examples" $ do
      intToDigits @Integer 1234
        `shouldBe` [1,2,3,4]
      intToDigits @Int 8
        `shouldBe` [8]
      intToDigits @Integer 43212345566
        `shouldBe` [4,3,2,1,2,3,4,5,5,6,6]

  describe "digitsToInt" $
    specify "identity" $
      property $
        \(Positive x) ->
          (digitsToInt . intToDigits @Int) x === x

  describe "intToDigitsRev" $
    specify "reversed intToDigits" $
      property $
        \(Positive x) ->
          intToDigits @Int x === reverse (intToDigitsRev x)

  describe "pick" $ do
    specify "empty" $
      pick @Bool [] `shouldBe` []
    specify "example" $
      pick @Int [4,1,2,3]
        `shouldBe` [(4,[1,2,3]),(1,[4,2,3]),(2,[4,1,3]),(3,[4,1,2])]
    specify "poly" $
      property $
        \(xs :: [OrdA]) ->
          let ys = pick xs
              sortedXs = sort xs
          in fmap fst ys === xs
             .&&. conjoin (fmap (\(h,t) -> sort (h:t) === sortedXs) ys)

  describe "numReverseInBase" $
    specify "examples" $ do
      numReverseInBase @Integer 16 0xAABBCCDD1234
        `shouldBe` 0x4321DDCCBBAA
      numReverseInBase @Int 2 0b11001011
        `shouldBe` 0b11010011
      numReverseInBase @Integer 10 987654321000
        `shouldBe` 123456789

  describe "pickInOrder" $
    specify "examples" $ do
      pickInOrder @() [] `shouldBe` []
      pickInOrder "ABCD" `shouldBe`
        [ ('A', "BCD")
        , ('B', "CD")
        , ('C', "D")
        , ('D', "")
        ]
