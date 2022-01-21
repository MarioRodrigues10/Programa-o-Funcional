import System.Random
import Data.Char
import Data.List
import Data.Maybe
import Data.String

bingo :: IO ()
bingo = do acc <- sorteio []
		   print acc

sorteio :: [Int] -> IO [Int]
sorteio l | length l == 90 = do return l 
 	      | otherwise = do x <- randomRIO (1,90)	
 	      				   print x
 	      				   getChar
 	      				   where
 	      				   let acc = if x `elem` l then l else x:l in sorteio acc

