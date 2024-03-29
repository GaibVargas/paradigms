alunos :: [(Int, String, Float)]
alunos = [(1, "Ana", 3.4), (2, "Bob", 6.7), (3, "Tom", 7.6)]

getNome :: (Int, String, Float) -> String
getNome (a,b,c) = b

getPrimeiroAluno :: [(Int, String, Float)] -> (Int, String, Float)
getPrimeiroAluno (a:_) = a

gerarPares :: [t] -> [u] -> [(t,u)]
gerarPares l1 l2 = [(a,b) | a <- l1, b <- l2]

notaAcimaMedia :: (Int, String, Float) -> Bool
notaAcimaMedia (_, _, nota) = nota >= 6

aprovados :: [(Int, String, Float)] -> [(Int, String, Float)]
aprovados lista = [aluno | aluno <- lista, notaAcimaMedia aluno]

main = do
    print (aprovados alunos)