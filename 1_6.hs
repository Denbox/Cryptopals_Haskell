import Utils
import System.IO
import Control.Monad
import Data.List

hammingForSize :: Int -> [Int] -> Float
hammingForSize n cipher = (hamming (take n cipher) (take n (drop n cipher))) / n

-- compareSnd = (\x y -> compare (snd x) (snd y))
--
-- -- keySize :: [Int] -> Int
-- -- keySize cipher = map (\x -> (x, hammingForSize)) [2..32]

main = do
  contents <- readFile "1_6.txt"
  let cipher = b64ToBin (filter (/= '\n') contents) -- remove new lines
  print cipher
  print $ hammingForSize 23 cipher
  -- print $ keySize cipher
