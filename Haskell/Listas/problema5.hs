busca :: [Int] -> Int -> Bool
busca [] _ = False
busca (a:b) elem | (a == elem) = True
	| otherwise = busca b elem

main = print(busca [1, 2, 3, 4] 3)