mdc a 0 = a
mdc a b = mdc b (mod a b)

mdc3 a b c = do
  if a > c && b > c then
    (mdc (mdc a b) c)
  else if a > b && c > b then
    (mdc (mdc a c) b)
  else
    (mdc (mdc b c) a)

main = do
  aString <- getLine
  bString <- getLine
  cString <- getLine
  let a = (read aString :: Integer)
  let b = (read bString :: Integer)
  let c = (read cString :: Integer)
  print (mdc3 a b c)