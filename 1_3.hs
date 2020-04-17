import Utils

main = do
  let input = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
  let cipher = binToAscii $ hexToBin input
  let keys = [' '..'~'] -- printable ascii
  print $ bestDecrypt (xorKeyExhaust cipher keys)
