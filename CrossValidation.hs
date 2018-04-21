{-# OPTIONS_GHC -F -pgmF htfpp #-}
module CrossValidation (nfold, htf_thisModulesTests) where
import Test.Framework
import Data.Function (on)
import qualified Data.List as List
import Data.List.Split (splitPlaces)
import System.Random
import System.Random.Shuffle

nfold :: (RandomGen gen, Ord s) =>
         [([x],y)] -> Int -> gen -> ([([x],y)] -> m) -> ([([x],y)] -> m -> s) -> (m, s)
nfold set n g createModel scoreModel =
    let l = length set
        rem = l `mod` n
        step = l `div` n
        nums = replicate (n - rem) step ++ replicate rem (step+1)
        shuffled = shuffle' set l g
        splits = splitPlaces nums shuffled
        tests = concat $ splits
        trains = [concat $ take a splits ++ drop (a+1) splits | a <- [0..n-1]]
        models = map createModel trains
        scores = map (scoreModel tests) models
        bestModel = List.maximumBy (compare `on` snd) $ zip models scores
    in bestModel
