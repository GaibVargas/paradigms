result media =
  if media >= 6 then
    "Aprovado"
  else
    "Reprovado"

media a b c = (a + b + c) / 3

main = do
  aString <- getLine
  bString <- getLine
  cString <- getLine
  let a = (read aString :: Float)
  let b = (read bString :: Float)
  let c = (read cString :: Float)
  print((result (media a b c)))