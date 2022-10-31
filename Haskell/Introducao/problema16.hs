main = do
  aString <- getLine
  bString <- getLine
  let a = (read aString :: Integer)
  let b = (read bString :: Integer)
  if a `mod` b == 0 then
    print ("Divisor")
  else
    print ("Nao divisor")