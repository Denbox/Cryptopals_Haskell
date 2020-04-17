import Utils
import Data.Char
import Data.List
import System.IO
import Control.Monad

asciiXor :: [Char] -> [Char] -> [Char]
asciiXor a b = binToAscii $ xor (asciiToBin a) (asciiToBin b)

keyExhaust :: [Char] -> [Char] -> [([Char], Char)]
keyExhaust cipher keys = map (\key -> (asciiXor cipher (cycle [key]), key)) keys

-- compareScore :: Ord ([Char], Char) => ([Char], Char) -> ([Char], Char) -> Ordering
compareScore decryptA decryptB = compare (englishScore $ fst decryptA) (englishScore $ fst decryptB)

bestDecrypt :: [([Char], Char)] -> ([Char], Char)
bestDecrypt candidates = maximumBy compareScore candidates

main = do
  contents <- readFile "1_4.txt"
  let ciphers = map (binToAscii . hexToBin) $ lines contents
  let keys = [' '..'~'] -- printable ascii
  let eachCipherBest = map (\cipher -> bestDecrypt (keyExhaust cipher keys)) ciphers
  print $ bestDecrypt eachCipherBest
