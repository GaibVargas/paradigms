ocorrenciaContador :: [Int] -> Int -> Int -> Int
ocorrenciaContador [] _ acc = acc
ocorrenciaContador (a:b) elem acc | (elem == a) = ocorrenciaContador b elem (acc + 1)
	| otherwise = ocorrenciaContador b elem acc

ocorrencia :: [Int] -> Int -> Int
ocorrencia lista elem = ocorrenciaContador lista elem 0

main = print(ocorrencia [2, 4, 2, 6, 2] 2)