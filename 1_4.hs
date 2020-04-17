import Utils
import System.IO
import Control.Monad

main = do
  contents <- readFile "1_4.txt"
  let ciphers = map (binToAscii . hexToBin) $ lines contents
  let keys = [' '..'~'] -- printable ascii
  let eachCipherBest = map (\cipher -> bestDecrypt (xorKeyExhaust cipher keys)) ciphers
  print $ bestDecrypt eachCipherBest
