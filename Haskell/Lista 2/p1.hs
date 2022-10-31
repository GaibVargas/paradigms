type Id = Int

type Nome = String

type Disciplina = String

type Nota = Float

type Aluno = (Int, Nome, Disciplina, Nota, Nota, Nota)



alunos :: [Aluno]

alunos = [(1, "Ana", "PC", 3.4, 6.2, 4.3), (2, "Bob", "PC", 6.7, 7.6, 7.7), (3, "Tom", "PC", 7.6, 9.1, 6.8)]


getAluno :: [Aluno] -> Int -> Aluno

getAluno ((id, nome, disciplina, nota1, nota2, nota3):b) n | (n == id) = (id, nome, disciplina, nota1, nota2, nota3)

	| otherwise = getAluno b n

getNome :: Aluno -> String
getNome (_, nome, _, _, _, _) = nome

getMedia :: Int -> Float
getMedia n = do 
	let (_, _, _, n1, n2, n3) = getAluno alunos n
	(n1 + n2 + n3)/3

comprimento :: [t] -> Int
comprimento [] = 0
comprimento (a:b) = 1 + comprimento b

acumulaMedia :: Int -> Float -> Float
acumulaMedia id acc = do
	let accIncrementado = acc + (getMedia id)
	if id < (comprimento alunos) then
		acumulaMedia (id + 1) accIncrementado
	else
		(accIncrementado / fromIntegral(comprimento alunos))

getMediaTurma :: Float
getMediaTurma = acumulaMedia 1 0 



main = print(getMediaTurma)