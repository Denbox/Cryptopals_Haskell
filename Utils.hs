module Utils
( hexToBin
, intToBin
, binToInt
, binToHex
, binToB64
, b64ToBin
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
, hamming
, depthHamming
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
hexToBin hex = concatMap ((zpad 4) . intToBin . hexToInt) hex

binToInt :: [Int] -> Int
binToInt bits = sum [fst x * (2^(snd x)) | x <- zip (reverse bits) [0..]]

splitLen :: Int -> [a] -> [[a]]
splitLen _ [] = []
splitLen n list
  | length list > n = [take n list] ++ splitLen n (drop n list)
  | otherwise = [take n list]

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

b64ToInt :: Char -> Int
b64ToInt b64 = fromJust $ elemIndex b64 b64Encoding

b64ToBin :: [Char] -> [Int]
b64ToBin b64 =
  let
    decoding = concatMap ((zpad 6) . intToBin . b64ToInt) $ takeWhile (/= '=') b64
    dropRemainder list = take ((div (length list) 8) * 8) list
  in
    dropRemainder decoding


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
-- englishScore string = sum (map (\x -> if elem x "ETAOINSHRDLUetaoinshrdlu " then 1 else 0) string)
-- englishScore string = sum (map (\x -> if elem (ord x) [32..126] then 1 else 0) string)
englishScore [] = 0
englishScore (x:xs)
  | x == ' ' = 8 + englishScore xs
  | elem x "ETAOINSHRDLUetaoinshrdlu" = 6 + englishScore xs
  | elem x (['A'..'Z'] ++ ['a'..'z']) = 3 + englishScore xs
  | elem x "?!.\"\'\n-" = 0 + englishScore xs
  | elem (ord x) [32..126] = -1 + englishScore xs
  | otherwise = -500 + englishScore xs

-- englishScore text = sum (map (\x -> fromEnum (elem x ([' '] ++ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']))) text)

xorKeyExhaust :: [Char] -> [Char] -> [([Char], Char)]
xorKeyExhaust ciphertext keys = map (\key -> (asciiXor ciphertext (cycle [key]), key)) keys

compareScore decryptA decryptB = compare (englishScore $ fst decryptA) (englishScore $ fst decryptB)

bestDecrypt :: [([Char], Char)] -> ([Char], Char)
bestDecrypt candidates = maximumBy compareScore candidates

hamming :: [Int] -> [Int] -> Int
hamming a b = sum $ xor a b

depthHamming :: Int -> [Int] -> Float
depthHamming key_size ciphertext =
  let
    chunks = splitLen key_size ciphertext
    hammings = zipWith hamming chunks (tail chunks)
  in
    (fromIntegral (sum hammings)) / (fromIntegral $ (length hammings) * key_size)
