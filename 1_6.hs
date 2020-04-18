import Utils
import System.IO
import Control.Monad
import Data.List
import Data.Char

compareSnd = (\x y -> compare (snd x) (snd y))
possible_keys = map chr [32..126]
set_up_xor_problems ciphertext key_size = transpose $ splitLen key_size ciphertext
solve_single_char_xors xor_problems = map (\x -> bestDecrypt $ xorKeyExhaust x possible_keys) xor_problems
decrypt ciphertext key_size = solve_single_char_xors $ set_up_xor_problems ciphertext key_size

main = do
  contents <- readFile "1_6.txt"
  let input = filter (/= '\n') contents -- remove new lines
  let ciphertext = b64ToBin input
  let possible_key_sizes = [2,3..40] -- in bytes
  let hammings_by_size = map (\n -> (n, depthHamming (n * 8) ciphertext)) possible_key_sizes
  let key_sizes = map fst (sortBy compareSnd hammings_by_size)
  let best_key_size = head key_sizes
  let readable_ciphertext = binToAscii ciphertext
  
  let decrypted_message_and_key = decrypt readable_ciphertext best_key_size
  let message = concat $ transpose $ map fst decrypted_message_and_key
  let key     =                      map snd decrypted_message_and_key
  putStrLn message
  putStrLn key
