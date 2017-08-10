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

\begin{code}

encode :: [String] -> ([String], [Int]) 
encode [] = error "Empty List"
encode list = encode' (encodeToWords list) (encodeToWordsNoDup list)

-- duplicates -> no duplicates -> number pattern
encode' :: [String] -> [String] -> ([String], [Int])
encode' duplicates nonDuplicates = (nonDuplicates, getPositionAll duplicates nonDuplicates [])

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
This code is very similar to question 1 a & b, doesn't care about duplicates
\begin{code}

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

splitWords':: String -> [Char] -> [String] -> [String]
splitWords' [] word list
    | length word > 0 =  list ++ word:[]
    | otherwise = list
splitWords' sentence word list
    | head sentence == ' ' = splitWords' (tail sentence) ([]) (list ++ word:[])
    | head sentence == ' ' = splitWords' (tail sentence) ([]) (list) --is duplicate
    | otherwise = splitWords' (tail sentence) (word ++ head sentence:[])  (list)

\end{code}


This code is very similar to question 1 a & b but instead checks if the newly formed word is already in our list before adding it
\begin{code}
encodeToWordsNoDup :: [String] -> [String] 
encodeToWordsNoDup [] = error "Empty List"
encodeToWordsNoDup xs = encodeToWordsNoDup' xs []

encodeToWordsNoDup' :: [String] -> [String] -> [String] 
encodeToWordsNoDup' [] rlist = rlist
encodeToWordsNoDup' sList rList = encodeToWordsNoDup' (tail sList) (splitWordsNoDup (head sList) rList)

splitWordsNoDup :: String -> [String] -> [String]
splitWordsNoDup word list
    | length word == 0 = []
    | otherwise = splitWordsNoDup' word [] list 

splitWordsNoDup':: String -> [Char] -> [String] -> [String]
splitWordsNoDup' [] word list
    | length word > 0 && checkDupl word list /= True =  list ++ word:[]
    | otherwise = list
splitWordsNoDup' sentence word list
    | head sentence == ' ' && checkDupl word list /= True = splitWordsNoDup' (tail sentence) ([]) (list ++ word:[])
    | head sentence == ' ' = splitWordsNoDup' (tail sentence) ([]) (list) --is duplicate
    | otherwise = splitWordsNoDup' (tail sentence) (word ++ head sentence:[])  (list)

--true if word is in list
checkDupl :: String -> [String] -> Bool 
checkDupl word [] = False
checkDupl word list
    | head list == word = True
    | otherwise = checkDupl word (tail list)

\end{code}

["the more I learn, the more I know.", "The more I know, the more I forget."] ["The", "more", "I", "learn", "the", "know.", "know,", "forget."] []