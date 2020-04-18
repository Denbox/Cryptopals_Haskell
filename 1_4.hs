import Utils
import System.IO
import Control.Monad

main = do
  contents <- readFile "1_4.txt"
  let ciphertexts = map (binToAscii . hexToBin) $ lines contents
  let keys = [' '..'~'] -- printable ascii
  let eachCipherBest = map (\ciphertext -> bestDecrypt (xorKeyExhaust ciphertext keys)) ciphertexts
  print $ bestDecrypt eachCipherBest
