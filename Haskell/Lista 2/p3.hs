data Ponto = D2 Float Float | D3 Float Float Float

distancia :: Ponto -> Ponto -> Float
distancia (D2 x1 y1) (D2 x2 y2) = sqrt (((x2 - x1) ** 2) + ((y2 - y1) ** 2))
distancia (D3 x1 y1 z1) (D3 x2 y2 z2) = sqrt (((x2 - x1) ** 2) + ((y2 - y1) ** 2) + ((z2 - z1) ** 2))

main = do
	print(distancia (D2 2 3) (D2 0 0))
	print(distancia (D3 4 8 6) (D3 0 2 6))