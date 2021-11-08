enum:: Int -> Int -> [Int]
enum x y = if x < y then x: enum (x+1) y
