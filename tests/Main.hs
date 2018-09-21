module Main where

-- import InspectionTests
-- import  InspectionTests.Basic
-- import  InspectionTests.Transformations
-- import  InspectionTests.Folds
-- import  InspectionTests.SpecialFolds
-- import  InspectionTests.AccumulatingMaps
-- import  InspectionTests.InfiniteLists
-- import  InspectionTests.Unfolding
-- import  InspectionTests.ExtractingSublists
-- import  InspectionTests.SublistsWithPredicates
-- import  InspectionTests.SearchingByEquality
-- import  InspectionTests.SearchingWithPredicate
-- import  InspectionTests.Indexing
-- import  InspectionTests.Zipping

import Prelude
import Consy
import Data.List
-- import Data.Text (Text)

main :: IO ()
-- main = pure ()
-- main = print $ Consy.take 10 (Consy.repeat 'a' :: Text)



{- testing transpose -}
-- main =
--   do
--     putStrLn $ "transpose [[1], [4,5,6], [7,8,9]]"
--     putStrLn $ "with consy"
--     print $ Consy.transpose [[1], [4,5,6], [7,8,9]]
--     putStrLn $ "with data.list"
--     print $ Data.List.transpose [[1], [4,5,6], [7,8,9]]
--     putStrLn $ "============="
--     putStrLn $ "transpose [[1,2,3], [4], [7,8]]"
--     putStrLn $ "with consy"
--     print $ Consy.transpose [[1,2,3], [4], [7,8]]
--     putStrLn $ "with data.list"
--     print $ Data.List.transpose [[1,2,3], [4], [7,8]]
--     putStrLn $ "============="
--     putStrLn $ "transpose [[1,2,3], [], [7,8]]"
--     putStrLn $ "with consy"
--     print $ Consy.transpose [[1,2,3], [], [7,8]]
--     putStrLn $ "with data.list"
--     print $ Data.List.transpose [[1,2,3], [], [7,8]]
--     putStrLn $ "============="
--     putStrLn $ "transpose [[1,2,3], [4,5,6,66], []]"
--     putStrLn $ "with consy"
--     print $ Consy.transpose [[1,2,3], [4,5,6,66], []]
--     putStrLn $ "with data.list"
--     print $ Data.List.transpose [[1,2,3], [4,5,6,66], []]
--     putStrLn $ "============="
--     putStrLn $ "transpose [[], [], []]"
--     putStrLn $ "with consy"
--     print $ (Consy.transpose [[]] :: [[Int]])
--     putStrLn $ "with data.list"
--     print $ (Data.List.transpose [[], [], []] :: [[Char]])


{- testing subsequences -}
-- main =
--   do
--     putStrLn $ "subsequences [1,2,3]"
--     putStrLn $ "with consy"
--     print $ (Consy.subsequences [1,2,3] :: [[Int]])
--     putStrLn $ "with data.list"
--     print $ (Data.List.subsequences [1,2,3] :: [[Int]])
--     putStrLn $ "============="
--     putStrLn $ "subsequences [[1,2,3], [4], [7,8]]"
--     putStrLn $ "with consy"
--     print $ (Consy.subsequences [[1,2,3], [4], [7,8]] :: [[[Int]]])
--     putStrLn $ "with data.list"
--     print $ (Data.List.subsequences [[1,2,3], [4], [7,8]] :: [[[Int]]])
--     putStrLn $ "============="
--     putStrLn $ "subsequences [[], [], []]"
--     putStrLn $ "with consy"
--     print $ (Consy.subsequences [] :: [[Int]])
--     putStrLn $ "with data.list"
--     print $ (Data.List.subsequences [] :: [[Int]])
--     putStrLn $ "============="
--     putStrLn $ "subsequences [[], [], []]"
--     putStrLn $ "with consy"
--     print $ (Consy.subsequences [[], [], []] :: [[[Char]]])
--     putStrLn $ "with data.list"
--     print $ (Data.List.subsequences [[], [], []] :: [[[Char]]])


{- testing cycle -}
main =
  do
    putStrLn $ "cycle [1,2,3]"
    print $ take 5 (Consy.map (+10) (Consy.cycle [1,2,3]))
    print $ Data.List.take 5 (Data.List.map (+10) (Data.List.cycle [1,2,3]))
    -- inspect ('consCycle' === 'listCycle')
