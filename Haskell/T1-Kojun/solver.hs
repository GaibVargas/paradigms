module SolverModule where

import Data.List

import TypeModule

-- Filtra elementos do tabuleiro por um determinado grupo
filterByGroupId :: Board -> Int -> [([Int], Int)]
filterByGroupId board groupId = filter ((==groupId) . snd) (foldl1 (++) board)

-- Retorna os valores que já estão configurados para um grupo do tabuleiro
valuesInGroup :: Board -> Int -> [Int]
valuesInGroup board groupId = concat (filter single (map fst (filterByGroupId board groupId)))

-- Define e reduz a quantidade de possíveis escolhas para uma casa do tabuleiro
possibleChoices :: Board -> Board
possibleChoices board = map (map defineChoices) board
  where defineChoices (value, groupId) | (head value == 0) = ([1..(groupSize board groupId)] \\ (valuesInGroup board groupId), groupId)
                                       | otherwise = ((difference value (valuesInGroup board groupId)), groupId)

-- Retorna se um array é de um único elemento
single :: [Int] -> Bool
single [_] = True
single _ = False

-- Retorna a diferença entre dois arrays, caso o primeiro array seja de um elemento esse array é retornado
difference :: [Int] -> [Int] -> [Int]
difference a b | (single a) = a
               | otherwise = a \\ b

-- Retorna a quantia de casas de um determinado grupo
-- Portanto retorna o maior valor possível em determinado grupo
groupSize :: Board -> Int -> Int
groupSize board id = length [x | line <- board, x <- filter ((==id) . snd) line]

-- Gera novos tabuleiros com base na primeira casa do tabuleiro que possui múltiplas possibilidades
-- Para cada possibilidade um novo tabuleiro é gerado
generateBoards :: Board -> [Board]
generateBoards board = 
  [b | boardAux <- [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs],
       let b = zipWith zip boardAux (map (map snd) board)]
  where
    (rows1, row:rows2) = break (any (not . single)) (map (map fst) board)
    (row1, cs:row2) = break (not . single) row

-- Procura o primeiro tabuleiro gerado que é uma solução válida para o tabuleiro passado como entrada
search :: Board -> [Board]
search board | not (valid board) = []
             | all (all (single . fst)) board = [board]
             | otherwise = [g | values' <- generateBoards board, g <- search (possibleChoices values')]

-- Retorna se os vizinhos de um valor no array são diferentes ao valor analisado
-- A validade é verificada caso os vizinhos não sejam iguais ao valor analisado
-- Usado para garantir que números em casas adjacentes ortogonalmente não sejam iguais
validNeighbours :: [PossibleChoices] -> Bool
validNeighbours [a] = True
validNeighbours (a:b:c) | (single a) && (single b) = if a == b then False else validNeighbours (b:c)
                        | otherwise = validNeighbours (b:c)

-- Retorna se todo elemento em um array é menor que seu antecessor
-- Usado para verificar se os valores na coluna estão em ordem decrescente
decreaseByGroupColumn :: [PossibleChoices] -> Bool
decreaseByGroupColumn [a] = True
decreaseByGroupColumn (a:b:c) | (single a) && (single b) = if a < b then False else decreaseByGroupColumn (b:c)
                              | otherwise = decreaseByGroupColumn (b:c)

-- Agrupo os valores do tabuleiro por vizinhança que pertencem ao mesmo grupo, e estão na mesma coluna
groupChoicesByColumn ::  Board -> [[PossibleChoices]]
groupChoicesByColumn board = do
  [line | values <- (transpose board), line <- groupByGroupId values]
  where
    groupByGroupId = map (map fst) . groupBy(\x y -> snd x == snd  y)

-- Valida se o tabuleiro atual é válido
valid :: Board -> Bool
valid board = all (validNeighbours) (map (map fst) board) && -- Verifica os vizinhos horizontais
              all (validNeighbours) (transpose (map (map fst) board)) && -- Verifica os vizinhos verticais
              all (decreaseByGroupColumn) (groupChoicesByColumn board) -- Verifica o decrescimo nas colunas

-- Inicia o processo de solução do tabuleiro
solver :: Board -> Board
solver board = head (search (possibleChoices board))