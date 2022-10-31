alunos :: [(Int, String, Float)]
alunos = [(1, "Ana", 3.4), (2, "Bob", 6.7), (3, "Tom", 7.6)]

getNome :: (Int, String, Float) -> String
getNome (a,b,c) = b

getId :: (Int, String, Float) -> String
getId (a,b,c) = a

getPrimeiroAluno :: [(Int, String, Float)] -> (Int, String, Float)
getPrimeiroAluno (a:_) = a

ehDiferente :: (Int, String, Float) -> (Int, String, Float) -> Bool
ehDiferente a b = (getId a) /= (getId b)

gerarPares :: [(Int, String, Float)] -> [(Int, String, Float)] -> [(String, String)]
gerarPares l1 l2 = [((getNome a), (getNome b)) | a <- l1, b <- l2, ehDiferente a b]

main = do
    print (gerarPares alunos)