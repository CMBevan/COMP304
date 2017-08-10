Q1)
a) 
lineToWords' uses 3 arguments, first the String we are parsing, second is the current word and lastly a list of the words. 


\begin{code}
lineToWords :: String -> [String]
lineToWords word 
    | length word == 0 = []
    | otherwise = lineToWords' word [] [] 

lineToWords':: String -> [Char] -> [[Char]] -> [[Char]]
--finished parsing sentence
lineToWords' [] word list
    | length word > 0 =  list ++ word:[]
    | otherwise = list
--still parsing sentence, current word is empty
lineToWords' sentence [] list 
    | head sentence == ' ' = lineToWords' (tail sentence) ([]) (list)
    | head sentence == ',' = lineToWords' (tail sentence) ([]) (list)
    | head sentence == '!' = lineToWords' (tail sentence) ([]) (list)
-- still parsing sentence, have partially constructed word
lineToWords' sentence word list
    | head sentence == ' ' = lineToWords' (tail sentence) ([]) (list ++ word:[])
    | head sentence == ',' = lineToWords' (tail sentence) ([]) (list ++ word:[])
    | head sentence == '!' = lineToWords' (tail sentence) ([]) (list ++ word:[])
    | otherwise = lineToWords' (tail sentence) (word ++ head sentence:[])  (list)

\end{code}


b)
\begin{code}
linesToWords :: [String] -> [String] 
linesToWords [] = error "Empty List"
linesToWords xs = linesToWords' xs []

-- list of words to pass -> list of broken up words -> return list
linesToWords' :: [String] -> [String] -> [String] 
linesToWords' [] rlist = rlist --base case empty list
--                                       list of string   list of broken words
linesToWords' sList rList = linesToWords' (tail sList) (rList ++ lineToWords (head sList))

\end{code}

c)



Q3)

a)


/*get given a list of strings e.g. ["sdaf adsfads asdf", "dasf", "sdfdsf"] 
and returns the list with no duplicates and the Int positions */

This code is very similar to question 1 a & b but instead checks if the newly formed word is already in our list before adding it
\begin{code}
--encode :: [Strin] -> ([String], [Int]) 

encodeToWords :: [String] -> [String] 
encodeToWords [] = error "Empty List"
encodeToWords xs = encodeToWords' xs []

encodeToWords' :: [String] -> [String] -> [String] 
encodeToWords' [] rlist = rlist
encodeToWords' sList rList = encodeToWords' (tail sList) (splitWords (head sList) rList)

splitWords :: String -> [String] -> [String]
splitWords word list
    | length word == 0 = []
    | otherwise = splitWords' word [] list 

splitWords':: String -> [Char] -> [[Char]] -> [[Char]]
splitWords' [] word list
    | length word > 0 && checkDupl word list /= True =  list ++ word:[]
    | otherwise = list
splitWords' sentence word list
    | head sentence == ' ' && checkDupl word list /= True = splitWords' (tail sentence) ([]) (list ++ word:[])
    | head sentence == ' ' = splitWords' (tail sentence) ([]) (list) --is duplicate
    | otherwise = splitWords' (tail sentence) (word ++ head sentence:[])  (list)

--true if word is in list
checkDupl :: String -> [String] -> Bool 
checkDupl word [] = False
checkDupl word list
    | head list == word = True
    | otherwise = checkDupl word (tail list)

\end{code}
/*This one will go through each string in the list */
encodeList :: [String] -> 


/*gets the position of a string from a list (used after duplicates are removed) */
getPosition:: String -> [String] -> Int -> Int



finds the position of a string in a group of strings
for testing get place >getPlace "test7" ["how", "for", "test", "test7"] 1  

encode:: [String] -> ([String], [Int])
encode [] = error "empty list" 
--encode s = getOrder (encode s)

encode' :: [String] -> [String]
encode' s = s


--go through a list

--words to parse -> full list -> positions list
getOrder' :: [String] -> [String] -> [Int] -> [Int]
getOrder' [] _ numbers = numbers
getOrder' word list numbers = getOrder (tail list) (list) ((getOrder' (head list) list 1):[] ++ numbers)


\begin{code}

getPositionAll:: [String] -> [String] -> [Int] -> [Int]
getPositionAll [] _ numbers = numbers
getPositionAll sentence list numbers = getPositionAll (tail sentence) list (numbers ++ (getPlace (head sentence) list 1):[])


--used to find the place of one element 
-- word to find -> full list -> position
getPlace:: String -> [String] -> Int -> Int
getPlace word [] _ = error "Number Not Found" --end of full list
getPlace word list place
    | head list == word = place
    | otherwise = getPlace word (tail list) (place + 1) 

\end{code}

["the more I learn, the more I know.", "The more I know, the more I forget."] ["The", "more", "I", "learn", "the", "know.", "know,", "forget."] []