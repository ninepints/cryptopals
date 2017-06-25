module Data.List.Window where


-- | Move a \"window\" over a list, capturing the contents
-- of the window at each position.
--
-- >>> window 2 [1,2,3,4]
-- [[1,2],[2,3],[3,4]]

-- >>> window 5 [1,2,3,4]
-- []
window :: Integer -> [a] -> [[a]]
window n xs | n < 1 = error "Window size zero or negative"
            | length xs < n' = []
            | otherwise = take n' xs : window n (tail xs)
    where n' = fromIntegral n
