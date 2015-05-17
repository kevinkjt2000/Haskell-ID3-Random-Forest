{- Copyright 2015 Kevin Tindall

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DecisionTree (DecisionTree,
                     getDT,
                     scorer,
                     decider,
                     drawPrettyTree,
                     label,
                     attrIndex,
                     attrValue,
                     htf_thisModulesTests) where
import qualified Test.Framework as TF
import Data.Maybe
import Data.Tree
import Data.Tree.Pretty

scorer :: (Ord x, Eq y) => [([x], y)] -> DecisionTree x y -> Double
scorer testset tree =
    let outputs = map (\(xs,y) -> (decider xs tree, y)) testset
        correct = filter (\(x, b) -> case x of Just a -> a == b
                                               Nothing -> False)
                $ outputs
        numCorrect = fromIntegral $ length correct
        percentage = numCorrect / (fromIntegral $ length testset)
    in percentage

decider :: Ord x => [x] -> DecisionTree x y -> Maybe y
decider xs (DT tree) = case tree of
    Node (Label l) []             -> Just l
    Node (AttrIndex i) valueTrees -> find i (map DT valueTrees)
    where find _ [] = Nothing
          find i ((DT (Node (AttrValue v) [subtree])) : tail) =
              if v == (xs !! i)
              then decider xs (DT subtree)
              else find i tail

drawPrettyTree = drawVerticalTree

label :: label -> DecisionTree attribute label
label l = DT $ Node (Label l) []

attrIndex :: Int -> [DecisionTree attribute label] -> DecisionTree attribute label
attrIndex i subtrees = DT $ Node (AttrIndex i) (map getDT subtrees)

attrValue :: attribute -> [DecisionTree attribute label] -> DecisionTree attribute label
attrValue a subtrees = DT $ Node (AttrValue a) (map getDT subtrees)

data DecisionTreeNode attribute label =
    Label label
    | AttrIndex Int
    | AttrValue attribute
    deriving (Show)

newtype DecisionTree attribute label =
    DT {getDT :: Tree (DecisionTreeNode attribute label)}
    deriving (Show)
