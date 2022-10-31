mymax a b c = do
  let aux = max a b
  max aux c

main = do
  aString <- getLine
  bString <- getLine
  cString <- getLine
  let a = (read aString :: Float)
  let b = (read bString :: Float)
  let c = (read cString :: Float)
  print (mymax a b c)