menor :: [Int] -> Int
menor [] = 0
menor [a] = a
menor (a:b) = minimo a (menor b)

minimo :: Int -> Int -> Int
minimo a b | (a < b) = a
	| otherwise = b

maior :: [Int] -> Int
maior [] = 0
maior [a] = a
maior (a:b) = maximo a (maior b)

maximo :: Int -> Int -> Int
maximo a b | (a > b) = a
	|otherwise = b

diferencaMaiorMenor lista = (maior lista)-(menor lista)

main = print(diferencaMaiorMenor [5, 6, 4, 9, 2, 10])