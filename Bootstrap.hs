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
module Bootstrap (bootstrap, htf_thisModulesTests) where
import Test.Framework
import Data.List.Split (chunksOf)
import System.Random

--Given a standard training set D of size n, bagging generates m new training sets D_i, each of size n', by sampling from D uniformly and with replacement. By sampling with replacement, some observations may be repeated in each D_i. If n'=n, then for large n the set D_i is expected to have the fraction (1 - 1/e) (?63.2%) of the unique examples of D, the rest being duplicates.
bootstrap :: [d] -> Int -> Int -> StdGen -> [[d]]
bootstrap set m n' g =
    let n = length set
        rs = randomRs (0,(n-1)) g
    in chunksOf n' $ map (set !!) $ take (n'*m) $ rs
