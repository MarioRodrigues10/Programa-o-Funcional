group:: Eq a => [a] -> [[a]]
group [] = []
group [h] = [[h]]
group (h:t) = let ((x:xs):ys) = group t
                     in if h == x then (h:x:xs):ys
                        else [h] : (x:xs):ys