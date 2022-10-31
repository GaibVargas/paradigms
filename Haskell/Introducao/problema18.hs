operate op a b
  | (op == "+") = a + b
  | (op == "-") = a - b
  | (op == "*") = a * b
  | (op == "/") = a / b

main = do
  aString <- getLine
  opString <- getLine
  bString <- getLine
  let a = (read aString :: Float)
  let b = (read bString :: Float)
  print (operate opString a b)