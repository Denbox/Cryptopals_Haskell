import Utils

main = do
  let input1 = "1c0111001f010100061a024b53535009181c"
  let input2 = "686974207468652062756c6c277320657965"
  let bin1   = hexToBin input1
  let bin2   = hexToBin input2
  let result = binToHex $ xor bin1 bin2
  print "Expected: 746865206b696420646f6e277420706c6179"
  print ("Result:   " ++ result)
