import Utils
import System.IO
import Control.Monad
import Data.List
import Data.Char

compareSnd = (\x y -> compare (snd x) (snd y))

possible_keys = map chr [32..126]
set_up_xor_problems ciphertext key_size = transpose $ splitLen key_size ciphertext
solve_single_char_xors xor_problems = map (\x -> fst (bestDecrypt $ xorKeyExhaust x possible_keys)) xor_problems
decrypt ciphertext key_size = concat $ transpose $ solve_single_char_xors $ set_up_xor_problems ciphertext key_size

main = do
  contents <- readFile "1_6.txt"
  let ciphertext = b64ToBin (filter (/= '\n') contents) -- remove new lines
  let possible_key_sizes = [16,24..320] -- 2 to 40 bytes
  let hammings_by_size = map (\n -> (n, depthHamming n ciphertext)) possible_key_sizes
  let key_sizes = map fst (sortBy compareSnd hammings_by_size)
  let readable_ciphertext = binToAscii ciphertext
  let solution = decrypt readable_ciphertext (head key_sizes)
  putStr solution
  print $ last solution
