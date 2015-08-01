-- Solution to Simultaneous Linear Equations using Matrices


import Matrix


solveEqns (Matrix coeff) (Matrix const) = (inverse (Matrix coeff)) |><| (Matrix const)


main = do
    putStrLn "\nEnter the coefficient matrix -"
    mat <- getLine
    putStrLn "\nEnter the constant matrix -"
    con <- getLine
    putStrLn ("\n\nSolution matrix -\n" ++ show (solveEqns (Matrix $ read mat) (Matrix $ read con)))




{-
Output - 2 equations and 2 variables
------------------------------------

[rohitjha@rohitjha Implementation]$ runhaskell lineq.hs

Enter the coefficient matrix -
[[1,2],[1,1]]

Enter the constant matrix -
[[4],[1]]


Solution matrix -
-2.0
3.0

-}


{-
Output - 2 equations and 2 variables
------------------------------------

[rohitjha@rohitjha Implementation]$ runhaskell lineq.hs

Enter the coefficient matrix -
[[4,3],[5,12]]

Enter the constant matrix -
[[7],[13]]


Solution matrix -
1.3636363636363635
0.5151515151515151

-}


{-
Output - 3 equations and 2 variables (No Solution)
--------------------------------------------------

[rohitjha@rohitjha Implementation]$ runhaskell lineq.hs

Enter the coefficient matrix -
[[1,1],[2,2],[3,3]]

Enter the constant matrix -
[[2],[4],[6]]


Solution matrix -
lineq.hs: Prelude.head: empty list

-}
