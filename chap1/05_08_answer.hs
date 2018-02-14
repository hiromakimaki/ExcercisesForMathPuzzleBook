{- *** From Q.05 to Q.08 answer ***

Prelude> :l 05_08_answer.hs
[1 of 1] Compiling Main             ( 05_08_answer.hs, interpreted )
Ok, one module loaded.
*Main> main
"*** Q.05 ***"
3518437540
"*** Q.06 ***"
26882
"*** Q.07 ***"
17368162415924
"*** Q.08 ***"
19

-}


-- Q.05 --
calcPascalsTriangleNums :: Int -> [Int]
calcPascalsTriangleNums n
    | n == 0 = [1]
    | otherwise = map (\x -> fst x + snd x) (zip (0:prevNums) (reverse (0:prevNums)))
        where prevNums = calcPascalsTriangleNums (n-1)

getSumOfBillsAndCoins :: Int -> Int
getSumOfBillsAndCoins m = sum (map fst [n10000, n5000, n2000, n1000, n500 ,n100, n50, n10, n5, n1]) where
    n10000 = divMod m 10000
    n5000 = divMod (snd n10000) 5000
    n2000 = divMod (snd n5000) 2000
    n1000 = divMod (snd n2000) 1000
    n500 = divMod (snd n1000) 500
    n100 = divMod (snd n500) 100
    n50 = divMod (snd n100) 50
    n10 = divMod (snd n50) 10
    n5 = divMod (snd n10) 5
    n1 = divMod (snd n5) 1

calcMoneySumForPascalsTriangeNums :: Int -> Int
calcMoneySumForPascalsTriangeNums n = sum (map getSumOfBillsAndCoins (calcPascalsTriangleNums n))


-- Q.06 --
calcSquareNum :: Int -> Int -> Int
calcSquareNum w h
    | (w <= 0) || ( h <= 0) = 0
    | w == 1 = h
    | h == 1 = w
    | w > h = calcSquareNum (w - h) h + 1
    | otherwise = calcSquareNum w (h - w) + 1

calcCasesForFixedSquareNumAndLongLength :: Int -> Int -> Int
calcCasesForFixedSquareNumAndLongLength squareNum longLen
    | (squareNum <= 0) || (longLen <= 0) = 0
    | otherwise = length (filter (\x -> x == squareNum) (map (calcSquareNum longLen) [1..longLen]))


-- Q.07 --
permutation :: Int -> Int -> Int
permutation n k
    | (n <= 0) || (k <= 0) = 1
    | otherwise = (n - k + 1) * (permutation n (k - 1))

calcSumOfMovement :: Int -> Int
calcSumOfMovement n =
    sum (map (\fixN -> (n - fixN) * fixN * (permutation n (n - fixN - 1))) [1..n])


-- Q.08 --
calcOneStrokePatternNum :: Int -> Int -> Int
calcOneStrokePatternNum w l = w + l - 1


main = do
    print $ "*** Q.05 ***"
    print $ calcMoneySumForPascalsTriangeNums 45
    
    print $ "*** Q.06 ***"
    print $ sum (map (calcCasesForFixedSquareNumAndLongLength 20) [1..1000])
    
    print $ "*** Q.07 ***"
    print $ calcSumOfMovement 15
    
    print $ "*** Q.08 ***"
    print $ calcOneStrokePatternNum 10 10

