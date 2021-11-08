isPrefix:: Eq a => [a] -> [a] -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (x:xs) (y:ys)  = x == y && isPrefix xs ys


