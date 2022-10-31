nFibonacci 1 = 0
nFibonacci 2 = 1
nFibonacci n = nFibonacci(n - 1) + nFibonacci(n - 2)

main = do
  nString <- getLine
  let n = (read nString :: Float)
  print(nFibonacci n)