All high order functions for 3 hours lol

Doesn't seem like a Lindsey exam - according to the Tutor

1)
a) A function that takes a function or returns a function - the idea of first order in practice

b) No explicit recursion
If it contains lists you start using folds and maps

i) anything in Num has from integer, 
\begin{code}

sumList' :: Num a => [a] -> a
sumList' l = foldr (+) (fromInteger 0) l
\end{code}


ii)

--
rule of thumb for using high order function:
1) list -> list you probably need a map
2) otherwise list -> a (single element) is a fold
--

\begin{code}
length' :: [a] -> Integer 
length' l = foldr (+) 0 (map (\_ -> 1)l)
\end{code}
iii)
Tutor read it as convert to a string - make Showable, only 4 marks so not asking for a lot

c) 

2)

a) 

ii) how does Haskell protect functions inside modules
comparision question, modules ar