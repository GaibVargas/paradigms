mapear :: (t -> y) -> [t] -> [y]
mapear f [] = []
mapear f (a:b) = (f a) : (mapear f b)

lista :: [Int]
lista = [1 .. 10]

dobrar :: Int -> Int
dobrar a = 2 * a

main = print(mapear dobrar lista)