main = do
  aString <- getLine
  bString <- getLine
  let a = (read aString :: Float)
  let b = (read bString :: Float)
  print (a ** b)