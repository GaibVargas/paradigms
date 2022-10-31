isPrimeLoop :: Integer -> Integer -> Bool
isPrimeLoop n comp =
  if (fromIntegral comp) > (sqrt (fromIntegral n)) then
    True
  else if n `mod` comp == 0 then
    False
  else
    (isPrimeLoop n (comp + 2))

isPrime :: Integer -> Bool
isPrime n =
  if n == 2 || n == 3 then
    True
  else if n `mod` 2 == 0 || n `mod` 3 == 0 then
    False
  else
    (isPrimeLoop n 5)

main = do
  xString <- getLine
  let x = (read xString :: Integer)
  if (isPrime x) then
    print("Primo")
  else
    print("Composto")