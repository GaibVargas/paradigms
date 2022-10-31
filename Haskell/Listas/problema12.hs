apagar :: Int -> [t] -> [t]
apagar n [] = []
apagar n lista = apagarN n lista 0

apagarN :: Int -> [t] -> Int -> [t]
apagarN _ [] _ = []
apagarN n (a:b) idx | (idx + 1 < n) = apagarN n b (idx + 1)
  | otherwise = b

main = print(apagar 1 [1..10])