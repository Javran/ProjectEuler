{-# LANGUAGE
    ScopedTypeVariables
  #-}
module ProjectEuler.SolCommonSpec where

import Control.Monad
import Petbox
import Test.Hspec
import Test.QuickCheck hiding (choose)
import Test.QuickCheck.Poly

import ProjectEuler.SolCommon

spec :: Spec
spec = do
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
  describe "slidingWindows" $
    specify "examples" $ do
      slidingWindows 3 "abcde"
        `shouldBe` ["abc", "bcd", "cde"]
      slidingWindows 4 "abcd"
        `shouldBe` ["abcd"]
      slidingWindows 10 "a"
        `shouldBe` []
  describe "choose" $ do
    specify "examples" $
      choose (37 :: Integer) 11 `shouldBe`
        (product (take 11 [37,36..]) `div` product [1..11])
    specify "small" $
      forM_ [2..10] $ \n -> do
        let xs = (n `choose`) <$> [0..n]
        take 3 xs `shouldBe` [1, n, n*(n-1) `div` 2]
        xs `shouldBe` reverse xs
        sum xs `shouldBe` 2 ^! n
