myWords :: String -> [String]
myWords = myWords' [] where
	myWords' :: [String] -> String -> [String]
	myWords' acc [] = acc
	myWords' acc (c:cs)
| isSpace c = myWords' acc cs
| otherwise = myWords' (acc ++ [word]) rest
where (word, rest) = span (/= ' ') (c:cs)

myReverse' :: [a] -> [a]
myReverse' = go []  where
    go :: [a] -> [a] -> [a]    go _ [] = id
    go acc (x:xs) = go (x:acc) xs

reverseWords :: String -> String
reverseWords = unwords . map myReverse . myWords

