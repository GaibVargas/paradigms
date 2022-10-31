mdc a 0 = a
mdc a b = mdc b (mod a b) -- mdc b (a % b)

coprimo a b =
  if (mdc a b) == 1 then
    True
  else
    False

qtdCoprimo a b acc =
  if b < a then
    if (coprimo a b) then
      (qtdCoprimo a (b + 1) (acc + 1))
    else
      (qtdCoprimo a (b + 1) acc)
  else
    acc

main = do
  aString <- getLine
  let a = (read aString :: Integer)
  print (qtdCoprimo a 1 0)