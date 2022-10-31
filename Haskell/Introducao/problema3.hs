triangleArea base height = (base * height) / 2

main = do
  baseString <- getLine
  heightString <- getLine
  let base = (read baseString :: Float)
  let height = (read heightString :: Float)
  print (triangleArea base height)