import Data.List(elemIndices)
import Data.Char(isHexDigit)

oneOrTwo :: Char -> Bool 
oneOrTwo x = x `elem` "10";

isBin :: String -> Bool
isBin [] = False
isBin (n:[]) = oneOrTwo n
isBin (n:rest) = isBin [n] && isBin rest

isBin' :: String -> Bool
isBin' x = length (filter (\x -> not (oneOrTwo x)) x) == 0



auxBin2Dec :: [Int] -> Int -> Int
auxBin2Dec (bit:[]) exp = bit*(2^exp)
auxBin2Dec (bit:bits) exp = (auxBin2Dec [bit] exp) + (auxBin2Dec bits (exp-1))


bin2dec :: [Int] -> Int
bin2dec [] = undefined
bin2dec bits = auxBin2Dec bits ((length bits)-1)

bin2dec' :: [Int] -> Int
bin2dec' bits = sum (map (2^) (elemIndices 1 (reverse bits))) -- Eu acho isso uma bagunca :/



dec2bin :: Int -> [Int]
dec2bin 0 = [] 
dec2bin dec =  dec2bin (dec `div` 2) ++ [dec `mod` 2]


isHex :: String -> Bool
isHex s = length (filter isHexDigit s) == length s

