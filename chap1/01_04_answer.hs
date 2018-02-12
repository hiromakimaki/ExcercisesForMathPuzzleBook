{- *** From Q.01 to Q.04 answer ***

Prelude> :l 01_04_answer.hs
[1 of 1] Compiling Main             ( 01_04_answer.hs, interpreted )
Ok, one module loaded.
*Main> main
"*** Q.01 ***"
5100
"*** Q.02 ***"
36863
"*** Q.03 ***"
93
"*** Q.04 ***"
8360

-}


-- Q.01 --
calcHandsPattern :: Int -> [[Int]]
calcHandsPattern n = [[g, c, n - g - c] |
        g <- [0..n],
        c <- [0..n],
        g + c <= n,
        not ((g == c) && (c == n - g - c)),
        not ((g == c) && (g > n - g - c)),
        not ((g == n - g - c) && (g > c)),
        not ((c == n - g - c) && (c > g))]


-- Q.02 --
calcStampPattern :: Int -> [[Int]]
calcStampPattern outN
    | outN < 2 = [[1]]
    | otherwise = map (\st -> (outN:st)) (concatMap calcStampPattern [1..(outN-1)])

calcStampPatternNumber :: Int -> Int -> Int
calcStampPatternNumber allN outN =
    sum (map (length . calcStampPattern) [outN, allN + 2 - outN]) - 1


-- Q.03 --
toRoman :: Int -> String
toRoman n = concat (map (\x -> createDigitExpression (snd x) (fst x)) (getOneToThousandDigits n))

getOneToThousandDigits :: Int -> [(Int, Int)]
getOneToThousandDigits n = map (\unit -> (mod (div n unit) 10, unit)) (map (\x -> 10^x) (reverse [0..3]))

createDigitExpression :: Int -> Int -> String
createDigitExpression unit d
    | (1 <= d) && (d <= 3) = concat (replicate d oneStr)
    | d == 4 = concat [oneStr, fiveStr]
    | (5 <= d) && (d <= 8) = concat (fiveStr:(replicate (d-5) oneStr))
    | d == 9 = concat [oneStr, tenStr]
    | otherwise = ""
    where 
        oneStr = case unit of
            1 -> "I"
            10 -> "X"
            100 -> "C"
            1000 -> "M"
            _ -> ""
        fiveStr = case unit of
            1 -> "V"
            10 -> "L"
            100 -> "D"
            _ -> ""
        tenStr = case unit of
            1 -> "X"
            10 -> "C"
            100 -> "M"
            _ -> ""

calcFixedLengthRomanNumberPattern :: Int -> [String]
calcFixedLengthRomanNumberPattern len = [toRoman x | x <- [1..3999], length (toRoman x) == len]


-- Q.04 --
getTimeIntDigits :: Int -> [(Int, Int)]
getTimeIntDigits n = map (\unit -> (mod (div n unit) 10, unit)) (map (\x -> 10^x) (reverse [0..5]))

getFilledSegmentsNumberSum :: Int -> Int
getFilledSegmentsNumberSum t =
    sum (map (\x -> getFilledSegmentsNumber (fst x)) (getTimeIntDigits t))
    where
        getFilledSegmentsNumber d
            | d == 1 = 2
            | d == 7 = 3
            | d == 4 = 4
            | d == 8 = 7
            | (d == 0) || (d == 6) || (d == 9) = 6
            | otherwise = 5

getFixedNumberSegmentsTimeIntPattern :: Int -> [Int]
getFixedNumberSegmentsTimeIntPattern n =
    [h * 10000 + m * 100 + s | h <- [0..23], m <- [0..59], s <- [0..59], getFilledSegmentsNumberSum (h * 10000 + m * 100 + s) == n]


main = do
    print $ "*** Q.01 ***"
    print $ length (calcHandsPattern 100)
    
    print $ "*** Q.02 ***"
    print $ calcStampPatternNumber 29 17
    
    print $ "*** Q.03 ***"
    print $ length (calcFixedLengthRomanNumberPattern 12)
    
    print $ "*** Q.04 ***"
    print $ length (getFixedNumberSegmentsTimeIntPattern 30)

