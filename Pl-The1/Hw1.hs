import Data.List

data Cell = H | P | O | X deriving (Read,Show,Eq)
data Result = Fail | Caught (Int,Int) deriving (Read,Show,Eq)
data Direction = N | S | E | W deriving (Read,Show,Eq)

simulate :: [[Cell]] -> [(Direction, Direction)] -> Result
-- implementation of simulate after that
simulate k l
 | hassimulate (head (elemIndices H (concat k)) )  (head (elemIndices P (concat k)) )   (elemIndices X (concat k))  l  (findRow k)  (length (concat k)) == (-1,-1) = Fail
 | otherwise = Caught ( hassimulate (head (elemIndices H (concat k)) )  (head (elemIndices P (concat k)) )   (elemIndices X (concat k))  l  (findRow k)  (length (concat k)) )
--Helper Functions
--For finding the row size
findRow :: [[Cell]] -> Int
findRow a  = length (a !! 0)
--For move operation
moveDirection :: Int -> Direction -> Int -> Int -> [Int] -> Int
moveDirection x y z t p
 | y == N 
  = if (x-z) >= 0 && not(elem (x-z) p)  
    then (x-z) 
    else  x 
 | y == S 
  = if (x+z) <= t-1 && not(elem (x+z) p)
    then (x+z) 
    else  x  
 | y == E 
  = if not((x+1) `mod` z == 0)  && not(elem (x+1) p)
    then (x+1) 
    else  x  
 | y == W 
  = if not(x `mod` z == 0) && not(elem (x-1) p)
    then (x-1) 
    else  x 
--Int to coordinate 
makeCoordinate :: Int -> Int -> (Int,Int)
makeCoordinate m n = (m`mod` n , (floor ((fromIntegral m)/(fromIntegral n))))
--need to take care
hassimulate :: Int -> Int -> [Int] -> [(Direction, Direction)] -> Int -> Int -> (Int,Int)
hassimulate a b c d e f
-- | moveDirection a  (fst(head(d)))   e   f   c  == moveDirection b  (snd(head(d)))  e  f   c  = makeCoordinate (moveDirection a  (fst(head(d)))   e   f   c) e
-- | not (moveDirection a  (fst(head(d)))   e   f   c  == moveDirection b  (snd(head(d)))  e  f   c ) && (d == []) = (-1,-1)
 | a == b = makeCoordinate a e
 | d == [] = (-1,-1)  
 | otherwise = hassimulate (moveDirection a  (fst(head(d)))   e   f   c) (moveDirection b  (snd(head(d)))   e   f   c) c (drop 1 d) e f





