filtrar :: (t -> Bool) -> [t] -> [t]
filtrar f [] = []
filtrar f lista = [x | x <- lista, (f x)]

ehPar :: Int -> Bool
ehPar n = n `mod` 2 == 0

main = print(filtrar ehPar [1..10])