mdc a 0 = a
mdc a b = mdc b (mod a b)

mmc a b = (a * b) `div` (mdc a b)

main = do
  aString <- getLine
  bString <- getLine
  let a = (read aString :: Integer)
  let b = (read bString :: Integer)
  print (mmc a b)