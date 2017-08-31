

--data Set = Set Int Set | Empty  deriving(Show)-- -- Set Float deriving(Show)

data Set a  = 
    Set{item :: El a
    ,nextValue:: Set (El a)}
    | Empty deriving (Show)

\begin{code}
data Set x = Empty | Value x (Set x) deriving (Eq, Show)  
--data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)  

\end{code}


This function makes a set but doesn't check for duplicates (so not really a set), this is used to initially test my
functions by creating simple sets for tests (That I manually make sure no duplicates) to make sure that set functions
work as intended
\begin{code}

testMakeSet :: [a] -> Set a
testMakeSet [] = Empty 
testMakeSet xs  
    | length (tail xs) == 0 = Value (head xs) Empty 
    | otherwise = Value (head xs) (testMakeSet (tail xs))
\end{code}


Checks if there is already an identical item inside the given Set, If it does find a duplicate it will return true early 
to speed up. Return True: is a duplicate, Return False: no duplicate 

To test it initially worked I used let t1 = testMakeSet [5,4,3,2,1] and went checkDup t1 x with x being values both being in the set and out
to make sure we got correct values
\begin{code}
checkDup :: (Ord a) => Set a -> a -> Bool
checkDup Empty _ = False

checkDup (Value s Empty) x --last value in set
    | s == x = True
    | otherwise = False

checkDup (Value s next) x --has an element in x
    | s == x = True
    | otherwise = checkDup next x 
--data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)  
\end{code}

\begin{code}
makeSet :: (Ord a) => [a] -> Set a
makeSet [] = Empty
makeSet list = makeSet' (tail list) (Value (head list) Empty) Empty

-- remaining list, head set -> end set -> return set (should be head at the end)
makeSet' :: (Ord a) => [a] -> Set a -> Set a -> Set a 
makeSet' list (Value x next) Empty  --first time called
    | (checkDup (Value x next)(head list)) == True && (length (tail list) == 0 ) = (Value x next) -- last element in list is a duplicate
    | (checkDup (Value x next)(head list)) == True = makeSet' (tail list) (Value x next) Empty
    | (checkDup (Value x next)(head list)) == False && (length (tail list) == 0 ) = (Value x (Value (head list) Empty)) -- last element in list is a duplicate
    | otherwise = makeSet' (tail list) (Value x (Value (head list) Empty)) Empty
\end{code}

For this function I have decided to go through the list and return the initial list if a duplicate is found 
instead of using checkDup functionpreviously defined since that would result in having to go through the 
set twice, first to check if there is a duplicate and then to add at the end
\begin{code}
add :: (Ord a) => a -> Set a -> Set a
add x Empty = Value x Empty --empty list given
add x set = add' x set False
--add to list function
--add x (Value xs Empty) | xs =

-- value -> List -> found -> return
add' :: (Ord a) => a -> Set a -> Bool -> Set a 



add' x (Value xs Empty) False
    | x == xs = (Value xs Empty)
    | otherwise = Value xs (Value x Empty)

add' x (Value xs Empty) True = Value xs Empty 

add' x (Value xs next) b
    | b == True = Value xs (add' x next True)
    | x == xs = Value xs (add' x next True)
    | otherwise = Value xs (add' x next False)
\end{code}