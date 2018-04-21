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
