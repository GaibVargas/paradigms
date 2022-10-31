primeiros :: Int -> [t] -> [t]
primeiros n [] = []
primeiros n lista = primeirosN n lista 0

primeirosN :: Int -> [t] -> Int -> [t]
primeirosN _ [] _ = []
primeirosN n (a:b) idx | (idx < n) =  a : primeirosN n b (idx + 1)
  | otherwise = []

main = print(primeiros 15 [1..10])