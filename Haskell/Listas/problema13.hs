apagarEnquanto :: (t -> Bool) -> [t] -> [t]
apagarEnquanto _ [] = []
apagarEnquanto f (a:b) | (f a) = apagarEnquanto f b
  | otherwise = a : b

par :: Int -> Bool
par n = n `mod` 2 == 0

main = print(apagarEnquanto par [1, 4, 6, 8])