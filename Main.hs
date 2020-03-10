import SampleVotes

getCandidates :: [[String]] ->  [(Int, String)]
getCandidates xs = zip [1..] (drop 2 (head xs))

cleanData :: [[String]] -> [[String]]
cleanData xs = filterRows (removeEveryNth (tail (init xs)) 21)

removeEveryNth :: [[String]] -> Int -> [[String]]
removeEveryNth xs n = take (n - 1) xs ++ removeEveryNth (drop n xs) n

filterRows :: [[String]] -> [[String]]
filterRows xxs = [drop 2 xs | xs <- xxs]