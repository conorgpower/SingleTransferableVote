import Data.List(sort,sortBy,group,groupBy)
import SampleVotes

getCandidates :: [[String]] ->  [(Int, String)]
getCandidates xs = zip [1..] (drop 2 (head xs))

cleanData :: [[String]] -> [[String]]
cleanData xs =  map removeAsterisks (filterRows (removeEveryNth (tail (init xs)) 21))

removeEveryNth :: [[String]] -> Int -> [[String]]
removeEveryNth xs n = take (n - 1) xs ++ removeEveryNth (drop n xs) n

filterRows :: [[String]] -> [[String]]
filterRows xss = [drop 2 xs | xs <- xss]

-- sortVotes :: [[String]] -> [[String]]
-- sortVotes xss = [sort xs | xs <- xss]

removeAsterisks :: [String] -> [String]
removeAsterisks vote = [x | x <- vote, x /= "*"]

-- ALTERNATIVE VOTING

-- Quota 
getAltQuota :: [[String]] -> Int
getAltQuota xs = length xs `div` 2 + 1

-- countVotes :: [[String]] -> [(String, Int)]
-- countVotes xs = 


-- SINGLE TRANSFERABLE VOTE