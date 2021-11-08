cola' :: [(String,b,c)] -> String
cola' [] = ""
cola' ((x,y,z):ys) = x ++ cola' ys