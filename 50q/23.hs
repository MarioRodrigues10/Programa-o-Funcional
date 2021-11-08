isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf [] _ = False
isSuffixOf _ [] = False
isSuffixOf l1 l2
    |l1 == l2 = True
    |l1 /= l2 = isSuffixOf l1 (tail l2)
    |otherwise = False