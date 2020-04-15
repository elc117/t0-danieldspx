oneOrTwo :: Char -> Bool 
oneOrTwo x = x `elem` "10";

isBin :: String -> Bool
isBin [] = False
isBin (n:[]) = oneOrTwo n
isBin (n:rest) = oneOrTwo n && isBin rest
