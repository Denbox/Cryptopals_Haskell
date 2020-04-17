import Utils
import Data.Char
import Data.List

third :: (a, b, c) -> c
third (a, b, c) = c

-- compare_snd x y = compare (snd x) (snd y)
compare_score x y = compare (third x) (third y)

main = do
  let input = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
  let binary = hexToBin input
  let possible_keys = map ((zpad 8) . intToBin) [32..126]
  let results = map (\key -> xor binary (cycle key)) possible_keys
  let readable_results = map binToAscii results
  let readable_keys = map binToAscii possible_keys
  let scores = map englishScore readable_results
  let results_keys_scores = zip3 readable_results readable_keys scores
  let ordered_results = sortBy compare_score results_keys_scores
  print $ last ordered_results
