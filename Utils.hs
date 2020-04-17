module Utils
( hexToBin
, intToBin
, binToInt
, binToHex
, binToB64
, binToAscii
, asciiToBin
, xor
, zpad
, splitLen
, englishScore
, asciiXor
, xorKeyExhaust
, compareScore
, bestDecrypt
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

zpad :: Int -> [Int] -> [Int]
zpad len bin = if length bin < len then zpad len (0:bin) else bin

hexToBin :: [Char] -> [Int]
hexToBin [] = []
hexToBin (x:xs) = zpad 4 (intToBin $ hexToInt x) ++ hexToBin xs

binToInt :: [Int] -> Int
binToInt [] = 0
binToInt bits = sum [fst x * (2^(snd x)) | x <- zip (reverse bits) [0..]]

splitLen :: Int -> [a] -> [[a]]
splitLen _ [] = []
splitLen n list = [take n list] ++ splitLen n (drop n list)

binToHex :: [Int] -> [Char]
binToHex bits =
  let
    ints = map binToInt $ splitLen 4 bits
    hex = map (\x -> hexEncoding !! x) ints
    -- end_pad_len = mod (length hex) FIGURE OUT HOW END PAD WORKS
  in
    hex

binToB64 :: [Int] -> [Char]
binToB64 bits =
  let
    ints = map binToInt $ splitLen 6 bits
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

asciiToBin :: [Char] -> [Int]
asciiToBin [] = []
asciiToBin (x:xs) = ((zpad 8) . intToBin . ord) x ++ asciiToBin xs

xor :: [Int] -> [Int] -> [Int]
xor a b = zipWith (\x y -> if x /= y then 1 else 0) a b

asciiXor :: [Char] -> [Char] -> [Char]
asciiXor a b = binToAscii $ xor (asciiToBin a) (asciiToBin b)

englishScore :: [Char] -> Int
englishScore string = sum (map (\x -> if elem x "ETAOINSHRDLUetaoinshrdlu " then 1 else 0) string)

xorKeyExhaust :: [Char] -> [Char] -> [([Char], Char)]
xorKeyExhaust cipher keys = map (\key -> (asciiXor cipher (cycle [key]), key)) keys

compareScore decryptA decryptB = compare (englishScore $ fst decryptA) (englishScore $ fst decryptB)

bestDecrypt :: [([Char], Char)] -> ([Char], Char)
bestDecrypt candidates = maximumBy compareScore candidates
