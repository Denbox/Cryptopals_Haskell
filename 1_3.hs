import Utils
import Data.Char
import Data.List

score :: [Char] -> Int
score string = sum (map (\x -> if elem x "aeiouAEIOUsStTrRnN " then 1 else 0) string)

compare_snd x y = compare (snd x) (snd y)

main = do
  let input = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
  let binary = hexToBin input
  let possible_keys = map ((zpadBin 8) . intToBin) [32..126]
  let results = map (\key -> xor binary (cycle key)) possible_keys
  let readable_results = map binToAscii results
  let scores = map score readable_results
  let results_and_scores = zip readable_results scores
  let best = fst $ maximumBy compare_snd results_and_scores
  print best
