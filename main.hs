module Main where
import Bootstrap
import CrossValidation
import DecisionTree
import ID3
import Data.Function (on)
import qualified Data.List as List
import Data.List.Split (splitOn)
import System.IO.Unsafe
import System.Random
import System.Environment
import System.Exit

{-  Program Assignment 1 Description from iLearn
Write a program that implements a Random Forest of unpruned ID3 trees. You can assume categorical data and no missing values. You do not need to know attribute names; you can call them att_1 … att_n or have an array of attributes. You can assume that the class is the first column of a comma-separated file. Start with the soybean-small.dat file attached to this assignment.

The program will take the number of trees and the data file name as an argument (runtime options) and output a confusion matrix produced using 10-fold crossvalidation. The Random Forest will use bootstrapping to select the training data for the tree and will random select an attribute subset of size N, where N is the first integer less than log2(M+1) and M is the number of available attributes. The out-of-bag accuracy estimate will be computed and output after the addition of each tree. The out-of-bag estimate will only include instances that do not appear in the training set for at least one of the trees. Thus, there will be some instances excluded from the out-of-bag estimate until there is a sufficient number of trees in your forest.
-}

data ConfMatrix a = CM {
  labels :: [a],
  matrix :: [[Int]]
} deriving (Show)

makeMatrix :: Ord a => [a] -> [a] -> ConfMatrix a
makeMatrix xs ys =
    CM vs [[count v1 v2 | v1 <- vs] | v2 <- vs]
    where zs = zip xs ys
          n  = length zs
          vs = List.sort $ List.nub (xs ++ ys)
          count a b = length $ filter (\(x,y) -> x==a && y==b) zs

usage = do name <- getProgName
           putStrLn $ "Usage: " ++ name ++ " <number of trees> <data filename>"
exit = exitWith ExitSuccess
die  = exitWith $ ExitFailure 1

createForest :: (Ord x, Ord y) => Int -> StdGen -> [([x],y)] -> [DecisionTree x y]
createForest numTrees gen dataset =
    let numData = length dataset
        datas = bootstrap dataset numTrees numData gen
        numAttrs = length $ fst $ head dataset
        logAttrs = floor $ (logBase 2 $ fromIntegral $ 1 + numAttrs :: Double)
        attrs = bootstrap ([0..numAttrs-1]) numTrees logAttrs gen
        forest = List.zipWith id3' datas attrs
    in unsafePerformIO $ do
        putStrLn $ "Out of bag accuracies(training): " ++ show (map (fst . onlyScoreForest dataset) $ tail $ map reverse $ scanl (flip (:)) [] forest)
        return forest

decideForest :: (Ord x, Ord y) => [x] -> [DecisionTree x y] -> Maybe y
decideForest xs forest =
    let votes = filter (/= Nothing) $ map (decider xs) forest
        group = List.groupBy (==) . List.sort
        majority = head . List.maximumBy (compare `on` length)
    in case votes of
        [] -> Nothing
        _ -> majority $ group votes

onlyScoreForest :: (Ord x, Ord y) => [([x],y)] -> [DecisionTree x y] -> (Double, ConfMatrix (Maybe y))
onlyScoreForest testset forest =
    let decisions = map (\(xs, y) -> (decideForest xs forest, y)) testset
        correct = filter (\(x, b) -> case x of Just a -> a == b
                                               Nothing -> False)
                $ decisions
        confusionMatrix = makeMatrix (map fst decisions) (map (Just . snd) testset)
        ilength = fromIntegral . length
        percentage = ilength correct / ilength testset
    in (percentage, confusionMatrix)

scoreForest :: (Ord x, Ord y, Show x, Show y) => [([x],y)] -> [DecisionTree x y] -> Double
scoreForest testset forest =
    let (percentage, confusionMatrix) = onlyScoreForest testset forest
    in unsafePerformIO $ do
        mapM putStrLn $ map (drawPrettyTree . fmap show . getDT) forest
        putStrLn $ show confusionMatrix
        putStrLn $ "Out of bag accuracies(test): " ++ show (map (fst . onlyScoreForest testset) $ tail $ map reverse $ scanl (flip (:)) [] forest)
        return percentage

main :: IO ()
main = do
    args <- getArgs
    if 2 /= length args
    then
        usage >> exit
    else do
        let num_trees = read $ args !! 0 :: Int
        let datafile = args !! 1
        contents <- readFile datafile
        let dataset = map (\(h:t) -> (t,h)) $ map (splitOn ",") $ lines contents
        g <- newStdGen
        let n = length dataset
        let (bestForest, bestScore) = nfold dataset 10 g (createForest num_trees g) scoreForest
        putStrLn $ "Best Accuracy: " ++ show bestScore
        exit
