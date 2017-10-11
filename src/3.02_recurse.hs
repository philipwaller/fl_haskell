
len2 :: [a] -> Int
len2 [] = 0
len2 (x:xs) = 1 + len2 xs

fil2 :: (a->Bool) -> [a] -> [a]
fil2 pred [] = []
fil2 pred (x:xs)
        | pred x        = x : fil2 pred xs
        | otherwise     = fil2 pred xs

map2 :: (a -> b) -> [a] -> [b]
map2 _ [] = []
map2 f (x:xs) = f x : map2 f xs

fldl2 :: (b -> a -> b) -> b -> [a] -> b
fld2 f z0 xs0 = lgo z0 xs0
        where
                lgo z [] = z
                lgo z (x:xs) = lgo (f z x) xs

-- NOTE: point free style
fldr2 :: (b->a->b) -> b -> [a] -> b
fldr2 k z = go
        where
                go [] = z
                go (x:xs) = x `k` go xs

-- 
fil2 pred = fgo where fgo [] = []; fgo (x:xs) | pred x = x : fgo xs | otherwise = fgo xs

