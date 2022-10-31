menor :: [Int] -> Int
menor [] = 0
menor [a] = a
menor (a:b) = minimo a (menor b)

minimo :: Int -> Int -> Int
minimo a b | (a < b) = a
	| otherwise = b

main = print(menor [5, 6, 4, 9, 2, 10])