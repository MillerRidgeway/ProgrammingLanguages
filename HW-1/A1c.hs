module A1c where 

  -- Finds a scaled dot product given a scalar and two 2D coordinates composed of integers
sDotProduct :: Integer -> (Integer, Integer) -> (Integer, Integer) -> Integer
sDotProduct scale p1 p2 = 
  let 
    newX = (fst p1) * (fst p2)
    newY = (snd p1) * (snd p2)
    sum = newX + newY
  in
    scale * sum

-- Finds the distance between two 2D coordiantes composed of integers
distance :: (Integer, Integer) -> (Integer, Integer) -> Float
distance p1 p2 = 
  let 
    x = ((fst p2) - (fst p1)) ^ 2
    y = ((snd p2) - (snd p1)) ^ 2
    sum = x + y 
  in 
    sqrt (fromInteger sum)

-- Finds the distance between two 3D coordiantes composed of integers 
tripleDistance :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)  -> Float
tripleDistance p1 p2 = 
  let 
    x1 = (\(x,_,_) -> x) p1
    y1 = (\(_,y,_) -> y) p1
    z1 = (\(_,_,z) -> z) p1
    
    x2 = (\(x,_,_) -> x) p2
    y2 = (\(_,y,_) -> y) p2
    z2 = (\(_,_,z) -> z) p2

    newX = (x2 - x1) ^ 2
    newY = (y2 - y1) ^ 2
    newZ = (z2 - z1) ^ 2
    
    sum = newX + newY + newZ
  in 
    sqrt (fromInteger sum)

-- Finds the minimum of the list of numbers
findMin :: [Integer] -> Integer 
findMin list = 
  if length list == 1 
    then head list
    else (\n1 n2 -> if n1 > n2 then n2 else n1) (head list) (findMin (tail list))

-- Finds the dot product between two lists of integers of equal length
tupleDotProduct :: [Integer] -> [Integer] -> Integer
tupleDotProduct list1 list2 = 
  if length list1 == 0 && length list2 == 0
    then 0 
    else (head list1) * (head list2) + tupleDotProduct (tail list1) (tail list2)

-- Zips two lists together reversing the order
revZip2Lists :: [a] -> [b] -> [(b,a)]
revZip2Lists list1 list2 = 
  if length list1 == 0 && length list2 == 0
    then []
    else [(last list2, last list1)] ++ revZip2Lists (init list1) (init list2)

-- Extracts every third value from a given list
everyThird :: [a] -> [a]
everyThird list =
  let 
    newList = snd (splitAt 3 list) 
  in
    if length list <= 2
      then []
      else [list !! 2] ++ everyThird newList

-- Extracts every Kth value from a given list
everyK :: Int -> [a] -> [a]
everyK k list = 
  let 
    indexedList = zip list [1..(length list)]
    filteredList = filter (\i -> (snd i) `mod` k == 0) indexedList
  in 
    map fst filteredList