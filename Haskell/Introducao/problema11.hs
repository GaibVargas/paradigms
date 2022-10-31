mdc a 0 = a
mdc a b = mdc b (mod a b) -- mdc b (a % b)

main = do
  aString <- getLine
  bString <- getLine
  let a = (read aString :: Integer)
  let b = (read bString :: Integer)
  print (mdc a b)