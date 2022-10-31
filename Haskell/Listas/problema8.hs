inverte :: [t] -> [t]
inverte [] = []
inverte (a:b) = (inverte b) ++ [a]

lista :: [Int]
lista = [1..10]

main = print(inverte lista)