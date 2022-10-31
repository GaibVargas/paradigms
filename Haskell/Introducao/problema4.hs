xor x y = (not x && y) || (x && not y)

main = do
  xString <- getLine
  yString <- getLine
  let x = (read xString :: Bool)
  let y = (read yString :: Bool)
  print (xor x y)