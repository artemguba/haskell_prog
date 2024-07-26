-- Функция, которая переворачивает строку
reverseString :: String -> String
reverseString str = reverseString' str ""

-- Вспомогательная функция с аккумулятором
reverseString' :: String -> String -> String
reverseString' "" acc = acc
reverseString' (x:xs) acc = reverseString' xs (x:acc)

-- Функция, которая переворачивает каждое слово в строке
reverseWords :: String -> String
reverseWords str = reverseWords' str "" ""

-- Вспомогательная функция с двумя аккумуляторами: для текущего слова и для результата
reverseWords' :: String -> String -> String -> String
reverseWords' "" word acc = acc ++ reverseString word
reverseWords' (x:xs) word acc
    | x == ' ' = reverseWords' xs "" (acc ++ reverseString word ++ " ")
    | otherwise = reverseWords' xs (word ++ [x]) acc

main :: IO ()
main = do
    let input = "abc def ghijk"
    let output = reverseWords input
    putStrLn output
