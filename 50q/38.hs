menor:: String -> String -> Bool
menor [] [] = False
menor _ [] = False
menor [] _ = True
menor (x:xs) (y:ys) = menor xs ys

{- 
"aaaa" "bbb"

menor "aaa" "bb"
menor "aa" "b"
menor "a" []
menor _ [] = False


-}