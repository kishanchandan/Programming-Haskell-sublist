len [] = 0  -- Length function 
len (x:xs) = 1 + len xs

osublist [] = [[]]  -- Base case
osublist x = list (array x) -- New function to perform operation on input array

list [[]] = []
list (x:xs)
  |mod (len x) 2== 0 = list xs
  |otherwise = x: list xs

array [] = [[]]
array (x:xs) = [x: temp | temp <- array xs] ++ array xs
