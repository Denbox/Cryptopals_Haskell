module Utils
( hexToBin
, intToBin
, binToInt
, binToHex
, binToB64
, binToAscii
, xor
, zpadBin
, splitLen
) where

import Data.Char
import Data.List
import Data.Maybe

hexEncoding = "0123456789abcdef"
b64Encoding = ['A'..'Z']++['a'..'z']++['0'..'9']++['+', '/']

hexToInt :: Char -> Int
hexToInt hex = fromJust $ elemIndex hex hexEncoding

intToBin :: Int -> [Int]
intToBin 0 = [0]
intToBin 1 = [1]
intToBin x = intToBin (quot x 2) ++ [rem x 2]

zpadBin :: Int -> [Int] -> [Int]
zpadBin len bin = if length bin < len then zpadBin len (0:bin) else bin

hexToBin :: [Char] -> [Int]
hexToBin [] = []
hexToBin (x:xs) = zpadBin 4 (intToBin $ hexToInt x) ++ hexToBin xs

binToInt :: [Int] -> Int
binToInt [] = 0
binToInt bits = sum [fst x * (2^(snd x)) | x <- zip (reverse bits) [0..]]

split4 :: [Int] -> [[Int]]
split4 [] = []
split4 (a:b:c:d:xs) = [[a,b,c,d]] ++ split4 xs
split4 lessThan4 = [lessThan4 ++ replicate (4 - length lessThan4) 0]

split6 :: [Int] -> [[Int]]
split6 [] = []
split6 (a:b:c:d:e:f:xs) = [[a,b,c,d,e,f]] ++ split6 xs
split6 lessThan6 = [lessThan6 ++ replicate (6 - length lessThan6) 0]

splitLen :: Int -> [a] -> [[a]]
splitLen _ [] = []
splitLen n list
  | n <= 0 = [list]
  | otherwise = [take n list] ++ splitLen n (drop n list)
--
-- pad_right_to :: Int -> a -> [a] -> [a]
-- pad_to n val list = list

binToHex :: [Int] -> [Char]
binToHex bits =
  let
    ints = map binToInt $ split4 bits
    hex = map (\x -> hexEncoding !! x) ints
    -- end_pad_len = mod (length hex) FIGURE OUT HOW END PAD WORKS
  in
    hex

binToB64 :: [Int] -> [Char]
binToB64 bits =
  let
    ints = map binToInt $ split6 bits
    b64 = map (\x -> b64Encoding!!x) ints
    end_pad_len = mod (length b64) 4
    end_pad = concat (replicate end_pad_len "=")
  in
    b64 ++ end_pad

binToAscii :: [Int] -> [Char]
binToAscii bits =
  let
    ints = map binToInt $ splitLen 8 bits
    ascii = map chr ints
  in
    ascii

xor :: [Int] -> [Int] -> [Int]
xor a b = zipWith (\x y -> if x /= y then 1 else 0) a b
