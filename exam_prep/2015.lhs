TUTORIAL 11
13-10-2017

1)
a) making sure you know how to use recursion

\begin{code}
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (a:as) (b:bs) = (a,b):(zip as bs)

\end{code}
b)
two base cases

i) the recursive case


last line is where actual recursion takes place
can talk about why we have 

ii) tail recursion, a parameter we pass through and then collect, building up a thing through recursion 
tail recursion is the key part of this
reverse at the end 

iii)
the idea is that in real recursion can use lazy evaluation (with tail recursion has all this extra computation) while
looking at the head. Lazy Evaluation is the key part

can write C & Java functionality - this is the key part of course, makes you a better programmer

First year question really - In java would just iterate through the list (as each element focus), recursion allows you
to break down into small bearable chunks

C)


\begin{code}
filter' :: Ord a -> (a -> Bool) -> [a] -> [a]
filter' f l = [a | a <- l, f a]
\end{code}


D) composing 


Q2)

b) fringe function

compare eager and lazy evaluation
Lazy is something you should think about 

look at left most leaves know immediately 