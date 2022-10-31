myabs x | (x >= 0) = x
        | otherwise = -x

main = do
  aString <- getLine
  let a = (read aString :: Float)
  print (myabs a)