module Helpers (
                dropSpaces
               , breakAt
               , nextElem
               )where

dropSpaces :: String -> String
dropSpaces = dropWhile (== ' ')

breakAt' :: (Eq a) => [[a]] -> a -> [a] -> [[a]]
breakAt' acc _ [] = reverse acc
breakAt' acc x (y:ys) 
    | x == y = breakAt' ([y]:acc) x ys
breakAt' acc x xs = let (pref,leftOver) = span (/= x) xs
                    in breakAt' (pref:acc) x leftOver

breakAt :: (Eq a) => a -> [a] -> [[a]]
breakAt = breakAt' []

nextElem :: Int -> [a] -> Maybe Int
nextElem idx xs = if (length xs - 1) > idx
                  then Just $ (idx + 1)
                  else Nothing