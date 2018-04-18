module Hw2 where

import Data.List -- YOU MAY USE THIS MODULE FOR SORTING THE AGENTS

data Level = Newbie | Intermediate | Expert deriving (Enum, Eq, Ord, Show, Read)
data Hunter = Hunter {hID::Int, hlevel::Level, hEnergy::Int, hNumberOfCatches::Int, hActions::[Direction]} deriving (Eq, Show, Read)
data Prey = Prey {pID::Int, pEnergy::Int, pActions::[Direction]} deriving (Eq, Show, Read)
data Cell = O | X | H Hunter | P Prey | T deriving (Eq, Show, Read)
data Direction = N | S | E | W deriving (Eq, Show, Read)
type Coordinate = (Int, Int)
-- DO NOT CHANGE THE DEFINITIONS ABOVE. --


-- INSTANCES OF Ord FOR SORTING, UNCOMMENT AND COMPLETE THE IMPLEMENTATIONS --
instance Ord Hunter where
 compare (Hunter hID1 hlevel1 hEnergy1 hNumberOfCatches1 hActions1) (Hunter hID2 hlevel2 hEnergy2 hNumberOfCatches2 hActions2)
   | (hlevel1 == Expert) && (hlevel2 == Intermediate) = GT
   | (hlevel1 == Expert) && (hlevel2 == Newbie) = GT
   | (hlevel1 == Intermediate) && (hlevel2 == Newbie) = GT
   | (hlevel1 == Intermediate) && (hlevel2 == Expert) = LT
   | (hlevel1 == Newbie) && (hlevel2 == Expert) = LT
   | (hlevel1 == Newbie) && (hlevel2 == Intermediate) = LT
   | otherwise =
      if (hEnergy1 > hEnergy2)
         then GT
         else if (hEnergy2 > hEnergy1)
            then LT
            else if(hNumberOfCatches1 > hNumberOfCatches2)
               then GT
               else if (hNumberOfCatches1 < hNumberOfCatches2)
                  then LT
                  else if (hID1 < hID2)
                     then GT
                     else LT
instance Ord Prey where
 compare (Prey pID1 pEnergy1 pActions1) (Prey pID2 pEnergy2 pActions2)
   | pEnergy1 > pEnergy2 = GT
   | pEnergy1 < pEnergy2 = LT
   | pEnergy1 == pEnergy2 =
      if (pID1 < pID2)
         then GT
         else LT

-- WRITE THE REST OF YOUR CODE HERE --
simulate  ::[[Cell]] -> ([(Hunter,Coordinate)],[(Prey,Coordinate)])
simulate cellList = hassimulate (find_Agents (is_Hunter) (concat cellList) (findRow cellList) 0) (find_Agents (is_Prey) (concat cellList) (findRow  cellList) 0) (findRow  cellList) (length cellList) (find_XT (is_Obstacle) (concat cellList) (findRow cellList) 0) (find_XT (is_Trap) (concat cellList) (findRow cellList) 0)


is_Hunter :: Cell -> Bool
is_Hunter (H x) = True
is_Hunter otherwise = False

is_Prey :: Cell -> Bool
is_Prey (P x) = True
is_Prey otherwise = False

is_Trap :: Cell -> Bool
is_Trap T = True
is_Trap otherwise = False

is_Obstacle :: Cell -> Bool
is_Obstacle X = True
is_Obstacle otherwise = False

--For finding the row size
findRow :: [[Cell]] -> Int
findRow a  = length (a !! 0)
--Int to coordinate
makeCoordinate :: Int -> Int -> (Int,Int)
makeCoordinate m n = (m`mod` n , (floor ((fromIntegral m)/(fromIntegral n))))
-- Find agents and their coordinates inputs -> is it the needed -> concat cell list -> rowSize -> 0 (starting index) 
find_Agents:: (Cell -> Bool) -> [Cell]->Int->Int->[(Cell,(Int,Int))]
find_Agents pred [] rowSize index= []
find_Agents pred (x:xs) rowSize index
   | pred x = (x,(makeCoordinate index rowSize)):find_Agents pred xs rowSize (index+1)
   | otherwise = find_Agents pred xs rowSize (index+1)
--Finding for trap and obstacle
find_XT:: (Cell -> Bool) -> [Cell]->Int->Int->[(Int,Int)]
find_XT pred [] rowSize index= []
find_XT pred (x:xs) rowSize index
   | pred x = ((makeCoordinate index rowSize)):find_XT pred xs rowSize (index+1)
   | otherwise = find_XT pred xs rowSize (index+1)
--Hunter eats the prey [use sort to send decreasing order prey list] sortBy (flip compare) for descending order
--update_Hunter:: Hunter -> [Prey] -> Int -> (Hunter,[Prey])
--update_Hunter ((Hunter a b hEnergy1 hNumberofCatches1 c)) [] count = ((Hunter a b hEnergy1 hNumberofCatches1 c),[]) 
--update_Hunter ((Hunter a b hEnergy1 hNumberofCatches1 c)) (x:xs) count 
--   | not (count == 2) = update_Hunter ((Hunter a b (hEnergy1+20) (hNumberofCatches1+1) c)) xs (count+1)
--   | otherwise = ((Hunter a b hEnergy1 hNumberofCatches1 c),(x:xs))

update_Hunter:: (Hunter,(Int,Int)) -> [(Prey,(Int,Int))] -> Int -> ((Hunter,(Int,Int)),[(Prey,(Int,Int))])
update_Hunter ((Hunter a b hEnergy1 hNumberofCatches1 c),(cordx,cordy)) [] count = (((Hunter a b hEnergy1 hNumberofCatches1 c),(cordx,cordy)),[]) 
update_Hunter ((Hunter a b hEnergy1 hNumberofCatches1 c),(cordx,cordy)) (x:xs) count 
   | (cordx,cordy) == (snd x) && not (count == 2) = update_Hunter ((Hunter a b (hEnergy1+20) (hNumberofCatches1+1) c),(cordx,cordy)) xs (count+1)
   | not((cordx,cordy) == (snd x)) && not (count == 2) =  
     if (hEnergy1 > 100 )
     then ((fst (update_Hunter ((Hunter a b (100) (hNumberofCatches1) c),(cordx,cordy)) (xs) (count))),([x]++ snd (update_Hunter ((Hunter a b (100) (hNumberofCatches1) c),(cordx,cordy)) (xs) (count))))
     else ((fst (update_Hunter ((Hunter a b (hEnergy1) (hNumberofCatches1) c),(cordx,cordy)) (xs) (count))),([x]++ snd (update_Hunter ((Hunter a b (hEnergy1) (hNumberofCatches1) c),(cordx,cordy)) (xs) (count))))
   | otherwise = 
     if (hEnergy1 > 100 )
     then (((Hunter a b 100 hNumberofCatches1 c),(cordx,cordy)),(x:xs))
     else (((Hunter a b hEnergy1 hNumberofCatches1 c),(cordx,cordy)),(x:xs))
--Preys and Hunters updated as lists
update_Agents:: [(Hunter,(Int,Int))]->[(Prey,(Int,Int))]->([(Hunter,(Int,Int))],[(Prey,(Int,Int))])
update_Agents [] [] = ([],[])
update_Agents hunterList [] = (hunterList,[])
update_Agents [] preyList = ([],preyList)
update_Agents (x:xs) preyList = ((fst (update_Hunter x preyList 0)):(fst (update_Agents xs (snd (update_Hunter x preyList 0)))) , (snd (update_Agents xs (snd (update_Hunter x preyList 0)))) ) 

--Move operations 
move_hunter:: (Cell,(Int,Int)) -> Int -> Int-> [(Int,Int)] -> (Cell,(Int,Int))
move_hunter (H(Hunter a b hEnergy1 d hActions1),(x,y)) rowSize coloumnSize obstacleList
 | (hActions1 !! 0) == N 
  = if (y > 0) && not(elem (x,y-1) obstacleList) && not(hEnergy1 < 10) && (hEnergy1 > 0)--obstacle  
      then  (H(Hunter a b (hEnergy1-1) d (drop 1 hActions1)),(x,y-1)) 
      else if (hEnergy1 < 10) --(y > 0) && not(elem (x,y) obstacleList) && (hEnergy1 < 10)
        then (H(Hunter a b (hEnergy1+1) d (drop 1 hActions1)),(x,y))
        else if (hEnergy1 > 0)
          then (H(Hunter a b (hEnergy1-1) d (drop 1 hActions1)),(x,y))
          else (H(Hunter a b (hEnergy1) d (drop 1 hActions1)),(x,y))
 | (hActions1 !! 0) == S 
  = if (y < (coloumnSize-1)) && not(elem (x,y+1) obstacleList) && not(hEnergy1 < 10) && (hEnergy1 > 0)
      then  (H(Hunter a b (hEnergy1-1) d (drop 1 hActions1)),(x,y+1)) 
      else if (hEnergy1 < 10) --(y < coloumnSize) && not(elem (x,y) obstacleList) && (hEnergy1 < 10)
        then (H(Hunter a b (hEnergy1+1) d (drop 1 hActions1)),(x,y))
        else if (hEnergy1 > 0)
          then (H(Hunter a b (hEnergy1-1) d (drop 1 hActions1)),(x,y))
          else (H(Hunter a b (hEnergy1) d (drop 1 hActions1)),(x,y))
 | (hActions1 !! 0) == E 
  = if (x < (rowSize-1) )  && not(elem (x+1,y) obstacleList) && not(hEnergy1 < 10) && (hEnergy1 > 0)
      then  (H(Hunter a b (hEnergy1-1) d (drop 1 hActions1)),(x+1,y)) 
      else if (hEnergy1 < 10) -- (x < rowSize ) && not(elem (x,y) obstacleList) && (hEnergy1 < 10)
        then (H(Hunter a b (hEnergy1+1) d (drop 1 hActions1)),(x,y))
        else if (hEnergy1 > 0)
          then (H(Hunter a b (hEnergy1-1) d (drop 1 hActions1)),(x,y))
          else (H(Hunter a b (hEnergy1) d (drop 1 hActions1)),(x,y))
 | (hActions1 !! 0) == W 
  = if (x > 0 ) && not(elem (x-1,y) obstacleList) && not(hEnergy1 < 10) && (hEnergy1 > 0)
      then  (H(Hunter a b (hEnergy1-1) d (drop 1 hActions1)),(x-1,y)) 
      else if (hEnergy1 < 10) --(x > 0 ) && not(elem (x,y) obstacleList) && (hEnergy1 < 10)
        then (H(Hunter a b (hEnergy1+1) d (drop 1 hActions1)),(x,y))
        else if (hEnergy1 > 0)
          then (H(Hunter a b (hEnergy1-1) d (drop 1 hActions1)),(x,y))
          else (H(Hunter a b (hEnergy1) d (drop 1 hActions1)),(x,y))
move_prey:: (Cell,(Int,Int)) -> Int -> Int-> [(Int,Int)] -> [(Int,Int)]-> (Cell,(Int,Int))
move_prey (P(Prey a pEnergy1 pActions1),(x,y)) rowSize coloumnSize obstacleList trapList
 | (pActions1 !! 0) == N 
  = if (y > 0) && not(elem (x,y-1) obstacleList) && not(elem (x,y-1) trapList) && not(pEnergy1 < 10) && (pEnergy1 > 0)--obstacle and trap
      then  (P(Prey a (pEnergy1-1) (drop 1 pActions1)),(x,y-1)) 
      else if (y > 0) && not(elem (x,y-1) obstacleList) && (elem (x,y-1) trapList) && not(pEnergy1 < 10) && (pEnergy1 > 0)
        then (P(Prey a (pEnergy1-11) (drop 1 pActions1)),(x,y-1)) 
        else if (elem (x,y) trapList) && ( not(y > 0) || (elem (x,y-1) obstacleList) || (pEnergy1 >= 11)) 
          then (P(Prey a (pEnergy1-11) (drop 1 pActions1)),(x,y))
          else if (elem (x,y) trapList) && ( not(y > 0) || (elem (x,y-1) obstacleList) || (pEnergy1 < 11)) && (pEnergy1 >= 0)
            then (P(Prey a (0) (drop 1 pActions1)),(x,y))
            else if (pEnergy1 < 10) --(y > 0) && not(elem (x,y) obstacleList) && (elem (x,y-1) trapList) && (pEnergy1 < 10)
              then (P(Prey a (pEnergy1+1) (drop 1 pActions1)),(x,y))
              else if (pEnergy1 > 0)
                then (P(Prey a (pEnergy1-1) (drop 1 pActions1)),(x,y))
                else (P(Prey a (pEnergy1) (drop 1 pActions1)),(x,y))
 | (pActions1 !! 0) == S 
  = if (y < (coloumnSize-1) ) && not(elem (x,y+1) obstacleList) && not(elem (x,y+1) trapList) && not(pEnergy1 < 10) && (pEnergy1 > 0)
      then  (P(Prey a (pEnergy1-1) (drop 1 pActions1)),(x,y+1)) 
      else if (y < (coloumnSize-1)) && not(elem (x,y+1) obstacleList) && (elem (x,y+1) trapList) && not(pEnergy1 < 10) && (pEnergy1 > 0)
        then (P(Prey a (pEnergy1-11) (drop 1 pActions1)),(x,y+1)) 
        else if (elem (x,y) trapList) && ( not(y < (coloumnSize-1)) || (elem (x,y+1) obstacleList) || (pEnergy1 >= 11)) 
          then (P(Prey a (pEnergy1-11) (drop 1 pActions1)),(x,y))
          else if (elem (x,y) trapList) && ( not(y < (coloumnSize-1)) || (elem (x,y+1) obstacleList) || (pEnergy1 < 11)) && (pEnergy1 >= 0)
          then (P(Prey a (0) (drop 1 pActions1)),(x,y))
          else if (pEnergy1 < 10) --(y < coloumnSize) && not(elem (x,y) obstacleList) && (elem (x,y+1) trapList) && 
            then (P(Prey a (pEnergy1+1) (drop 1 pActions1)),(x,y))
            else if (pEnergy1 > 0)
              then (P(Prey a (pEnergy1-1) (drop 1 pActions1)),(x,y))
              else (P(Prey a (pEnergy1) (drop 1 pActions1)),(x,y))
 | (pActions1 !! 0) == E 
  = if (x < (rowSize-1) )  && not(elem (x+1,y) obstacleList) && not(elem (x+1,y) trapList) && not(pEnergy1 < 10) && (pEnergy1 > 0)
      then  (P(Prey a (pEnergy1-1) (drop 1 pActions1)),(x+1,y)) 
      else if (x < (rowSize-1) )  && not(elem (x+1,y) obstacleList) && (elem (x+1,y) trapList) && not(pEnergy1 < 10) && (pEnergy1 > 0)
        then (P(Prey a (pEnergy1-11) (drop 1 pActions1)),(x+1,y)) 
        else if (elem (x,y) trapList) && ( not(x < (rowSize-1)) || (elem (x+1,y) obstacleList) || (pEnergy1 >= 11)) 
           then (P(Prey a (pEnergy1-11) (drop 1 pActions1)),(x,y))
           else if (elem (x,y) trapList) && ( not(x < (rowSize-1)) || (elem (x+1,y) obstacleList) || (pEnergy1 < 11)) && (pEnergy1 >= 0)
           then (P(Prey a (0) (drop 1 pActions1)),(x,y))
             else if (pEnergy1 < 10) --(x < rowSize ) && not(elem (x+1,y) obstacleList) && (elem (x+1,y) trapList) && (pEnergy1 < 10)
              then (P(Prey a (pEnergy1+1) (drop 1 pActions1)),(x,y))
              else if (pEnergy1 > 0)
                then (P(Prey a (pEnergy1-1) (drop 1 pActions1)),(x,y))
                else (P(Prey a (pEnergy1) (drop 1 pActions1)),(x,y))
 | (pActions1 !! 0) == W 
  = if (x > 0 ) && not(elem (x-1,y) obstacleList) && not(elem (x-1,y) trapList) && not(pEnergy1 < 10) && (pEnergy1 > 0)
      then  (P(Prey a (pEnergy1-1) (drop 1 pActions1)),(x-1,y)) 
      else if (x > 0 ) && not(elem (x-1,y) obstacleList) && (elem (x-1,y) trapList) && not(pEnergy1 < 10) && (pEnergy1 > 0)
        then (P(Prey a (pEnergy1-11) (drop 1 pActions1)),(x-1,y)) 
        else if (elem (x,y) trapList) && ( not(x > 0) || (elem (x-1,y) obstacleList) || (pEnergy1 >= 11)) 
          then  (P(Prey a (pEnergy1-11) (drop 1 pActions1)),(x,y))
          else if (elem (x,y) trapList) && ( not(x > 0) || (elem (x-1,y) obstacleList) || (pEnergy1 < 11)) && (pEnergy1 >= 0)
            then  (P(Prey a (0) (drop 1 pActions1)),(x,y))
            else if (pEnergy1 < 10) --(x > 0 ) && not(elem (x,y) obstacleList) && (elem (x-1,y) trapList) && (pEnergy1 < 10)
              then (P(Prey a (pEnergy1+1) (drop 1 pActions1)),(x,y))
              else if (pEnergy1 > 0)
                then (P(Prey a (pEnergy1-1) (drop 1 pActions1)),(x,y))
                else (P(Prey a (pEnergy1) (drop 1 pActions1)),(x,y))
--map all the move operations
map_hunter :: [(Cell,(Int,Int))] -> Int -> Int-> [(Int,Int)] -> [(Hunter,(Int,Int))]
map_hunter [] rowSize coloumnSize obstacleList = []
map_hunter (x:xs) rowSize coloumnSize obstacleList = (y,cord) : map_hunter xs rowSize coloumnSize obstacleList
    where ((H y),cord) = move_hunter x rowSize coloumnSize obstacleList

map_prey :: [(Cell,(Int,Int))] -> Int -> Int-> [(Int,Int)] -> [(Int,Int)]-> [(Prey,(Int,Int))]
map_prey [] rowSize coloumnSize obstacleList trapList = []
map_prey (x:xs) rowSize coloumnSize obstacleList trapList = (y,cord) : map_prey xs rowSize coloumnSize obstacleList trapList
    where ((P y),cord) = move_prey x rowSize coloumnSize obstacleList trapList

hassimulate ::  [(Cell,(Int,Int))] ->  [(Cell,(Int,Int))] -> Int -> Int-> [(Int,Int)] -> [(Int,Int)] -> ([(Hunter,(Int,Int))],[(Prey,(Int,Int))])
hassimulate [] [] rowSize coloumnSize obstacleList trapList  = ([],[])
hassimulate hunterList [] rowSize coloumnSize obstacleList trapList  = ((cell_to_hunter_list hunterList),[])
hassimulate hunterList preyList rowSize coloumnSize obstacleList trapList 
  | hActions (fst((cell_to_hunter_list hunterList) !! 0)) == [] = (sortBy (flip compare) (cell_to_hunter_list hunterList),sort (cell_to_prey_list preyList))
  |otherwise = hassimulate (hunter_to_cell_list (fst(update_Agents (sortBy (flip compare) (map_hunter hunterList rowSize coloumnSize obstacleList))  (sort (map_prey preyList rowSize coloumnSize obstacleList trapList))))) (prey_to_cell_list (snd (update_Agents (map_hunter hunterList rowSize coloumnSize obstacleList)  (map_prey preyList rowSize coloumnSize obstacleList trapList)))) rowSize coloumnSize obstacleList trapList

hunter_to_cell :: (Hunter,(Int,Int)) -> (Cell,(Int,Int))
hunter_to_cell (x,(a,b)) = ((H x),(a,b))

prey_to_cell   :: (Prey,(Int,Int))->(Cell,(Int,Int))
prey_to_cell (x,(a,b)) = ((P x),(a,b))

cell_to_hunter :: (Cell,(Int,Int))->(Hunter,(Int,Int))
cell_to_hunter ((H x),(a,b)) = (x,(a,b))


cell_to_prey :: (Cell,(Int,Int))->(Prey,(Int,Int))
cell_to_prey ((P x),(a,b)) = (x,(a,b))

hunter_to_cell_list :: [(Hunter,(Int,Int))] -> [(Cell,(Int,Int))]
hunter_to_cell_list [] = []
hunter_to_cell_list (x:xs) = (hunter_to_cell x):hunter_to_cell_list xs

prey_to_cell_list   :: [(Prey,(Int,Int))] -> [(Cell,(Int,Int))]
prey_to_cell_list [] = []
prey_to_cell_list (x:xs) = (prey_to_cell x):prey_to_cell_list xs

cell_to_hunter_list :: [(Cell,(Int,Int))] -> [(Hunter,(Int,Int))]
cell_to_hunter_list [] = []
cell_to_hunter_list (x:xs) = (cell_to_hunter x): cell_to_hunter_list xs

cell_to_prey_list :: [(Cell,(Int,Int))] -> [(Prey,(Int,Int))]
cell_to_prey_list [] = []
cell_to_prey_list (x:xs) = (cell_to_prey x): cell_to_prey_list xs







