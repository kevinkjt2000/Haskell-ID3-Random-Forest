{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where
import Test.Framework
import {-@ HTF_TESTS @-} Bootstrap
import {-@ HTF_TESTS @-} CrossValidation
import {-@ HTF_TESTS @-} DecisionTree
import {-@ HTF_TESTS @-} ID3

main :: IO ()
main = htfMain htf_importedTests
