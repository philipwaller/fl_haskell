maxhelper :: Int -> [Int] -> Int
maxhelper x [] = x
maxhelper x (y:ys)
        | x > y = maxhelper x ys
        | otherwise = maxhelper y ys

maxfromlist :: [Int] -> Maybe Int
maxfromlist [] = Nothing
maxfromlist (x:xs) = Just(maxhelper x xs)

