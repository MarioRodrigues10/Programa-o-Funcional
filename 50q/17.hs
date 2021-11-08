fun':: [(a,b,c)] -> [(a,c)]
fun' [] = []
fun' ((x,y,z):t) = (x,z): fun' t 