(defun board6 ()
  "Tabuleiro 6x6"
  '(
    (((0) 0) ((0) 1) ((4) 1) ((0) 1) ((2) 3) ((0) 4))
    (((0) 0) ((0) 2) ((3) 1) ((0) 3) ((0) 3) ((0) 3))
    (((1) 0) ((4) 0) ((0) 5) ((4) 3) ((0) 10) ((0) 10)) 
    (((0) 6) ((5) 7) ((0) 5) ((0) 9) ((0) 9) ((2) 10)) 
    (((0) 6) ((0) 7) ((0) 7) ((0) 8) ((3) 8) ((0) 10)) 
    (((6) 7) ((2) 7) ((0) 7) ((2) 8) ((0) 8) ((5) 8))
  )
)

(defun board8 ()
  "Tabuleiro 8x8"
  '(
    (((0) 0) ((6) 1) ((2) 2) ((0) 2) ((0) 3) ((0) 4) ((1) 4) ((2) 4))
    (((4) 1) ((3) 1) ((0) 2) ((0) 2) ((7) 5) ((0) 5) ((0) 5) ((0) 5))
    (((0) 1) ((1) 1) ((0) 1) ((0) 6) ((0) 6) ((0) 5) ((0) 5) ((0) 7))
    (((0) 8) ((0) 8) ((0) 8) ((0) 9) ((1) 10) ((0) 10) ((4) 5) ((0) 7))
    (((0) 11) ((0) 11) ((3) 8) ((0) 9) ((2) 9) ((5) 14) ((3) 18) ((0) 18))
    (((4) 11) ((0) 12) ((1) 12) ((0) 15) ((0) 14) ((3) 14) ((0) 17) ((0) 18))
    (((0) 11) ((0) 13) ((0) 13) ((0) 15) ((0) 14) ((0) 14) ((6) 14) ((0) 18))
    (((2) 11) ((5) 11) ((0) 16) ((3) 16) ((1) 16) ((2) 16) ((0) 16) ((1) 18))
  )
)

(defun indexBoard (board)
  "Indexa tabuleiro iniciando em 1"
  (let ((aux '()))
    (matrixMap (function (lambda (el all)
      (append el (list (len (push 0 aux))))
    )) board)
  )
)

(defun len (list)
  "Retorna o tamanho de uma lista"
  (if (null list)
    0
    (+ 1 (len (rest list)))
  )
)

(defun single (list)
  "Retorna se um array é de um único elemento"
  (= (len list) 1)
)

(defun filter (f list)
  "Função filter"
  (if (null list)
    ()
    (if (funcall f (first list))
      (cons (first list) (filter f (rest list)))
      (filter f (rest list))
    )
  )
)

(defun myMap (f list &optional all)
  "Função map"
  (if (null list)
    ()
    (if all
      (cons (funcall f (first list) all) (myMap f (rest list) all))
      (cons (funcall f (first list)) (myMap f (rest list)))
    )
  )
)

(defun matrixMap (f list &optional all)
  "Aplica uma função em todo item de uma matriz"
  (if (null list)
    ()
    (if all
      (cons (myMap f (first list) all) (matrixMap f (rest list) all))
      (cons (myMap f (first list) list) (matrixMap f (rest list) list))    
    )
  )
)

(defun getElemId (elem)
  "Retorna o identificador de grupo de um elemento do tabuleiro"
  (nth 1 elem)
)

(defun getElemValues (elem)
  "Retorna o primeiro elemento de uma lista. Retorna os valores de uma célula do tabuleiro"
  (nth 0 elem)
)

(defun getElemIdx (elem)
  "Retorna o índice de uma célula do tabuleiro"
  (nth 2 elem)
)

(defun getElemByIdx (board idx)
  "Retorna o elemento do tabuleiro de determinado índice"
  (loop for line in board
    do (loop for elem in line
      do (if (= (getElemIdx elem) idx) (return-from getElemByIdx elem))
    )
  )
)

(defun getBoardValues (board)
  "Retorna uma matriz com somente os possíveis valores para cada célula do tabuleiro"
  (matrixMap (lambda (elem all) (getElemValues elem)) board)
)

(defun filterById (id list)
  (if (null list)
    ()
    (if (= (getElemId (first list)) id)
      (cons (first list) (filterById id (rest list)))
      (filterById id (rest list))
    )
  )
)

(defun flatMatrix (matrix)
  (if (null matrix)
    ()
    (concatenate 'list (first matrix) (flatMatrix (rest matrix)))
  )
)

(defun filterByGroupId (board groupId)
  "Filtra elementos do tabuleiro por um determinado grupo"
  (let ((merged (flatMatrix board)))
    (filterById groupId merged)
  )
)

(defun valuesInGroup (board groupId)
  "Retorna os valores que já estão configurados para um grupo do tabuleiro"
  (let ((filtered (filterByGroupId board groupId)))
    (let ((elemValues (myMap (function getElemValues) filtered)))
      (myMap (function getElemValues) (filter (function single) elemValues))
    )
  )
)

(defun fillList (n)
  "Retorna uma lista com os valores de 1 até n"
  (filling 1 n)
)

(defun filling (start stop)
  "Função auxiliar para gerar lista de start até stop"
  (if (> start stop)
    ()
    (cons start (filling (+ start 1) stop))
  )
)

(defun neighboursValues (board elem)
  "Retorna lista com os valores das células vizinhas ao elemento"
  (let (
      (idx (getElemIdx elem))
      (aux)
    )
    (loop for index in (list (- idx 1) (+ idx 1) (- idx (len board)) (+ idx (len board)))
      do (let ((el (getElemByIdx board index)))
        (if el
          (if (single (getElemValues el))
            (push (getElemValues (getElemValues el)) aux)
          )
        )
      )
    )
    aux
  )
)

(defun defineChoices (elem board)
  "Define e reduz a quantidade de possíveis escolhas para uma casa do tabuleiro"
  (if (= (getElemValues (getElemValues elem)) 0)
    (let
      (
        (initialList (fillList (len (filterByGroupId board (getElemId elem)))))
        (existingValues (valuesInGroup board (getElemId elem)))
      )
      (list
        (reverse (difference (difference initialList existingValues) (neighboursValues board elem)))
        (getElemId elem)
        (getElemIdx elem)
      )
    )
    (list 
      (reverse (difference
        (difference (getElemValues elem) (valuesInGroup board (getElemId elem)))
        (neighboursValues board elem)
      ))
      (getElemId elem)
      (getElemIdx elem)
    )
  )
)

(defun difference (list1 list2)
  "Retorna a diferença entre dois arrays, caso o primeiro array seja de um elemento esse array é retornado"
  (if (= (len list1) 1)
    list1
    (filter (lambda (elem) (not (member elem list2))) list1)
  )
)

(defun possibleChoices (board)
  "Retorna tabuleiro com escolhas reduzidas"
  (matrixMap (function defineChoices) board)
)

(defun isEqual (elem1 elem2)
  "Retorna se duas células do tabuleiro são a mesma célula"
  (= (getElemIdx elem1) (getElemIdx elem2))
)

(defun sameArray (arr1 arr2)
  "Retorna se dois arrays são iguais a nível de conteúdo, não importa a ordem dos elementos"
  (= (len (difference arr1 arr2)) 0)
)

(defun validNeighbours (list)
  "Retorna se os vizinhos de um valor no array são diferentes ao valor analisado"
  (if (single list)
    T
    (let (
      (a (first list))
      (b (first (rest list)))
      (c (rest (rest list)))
      )
      (if (and (single a) (single b))
        (if (= (getElemValues a) (getElemValues b))
          NIL
          (validNeighbours (cons b c))
        )
        (validNeighbours (cons b c))
      )
    )
  )
)

(defun valid (board)
  "Verifica se o tabuleiro atual é válido"
  (and
    (every (function validNeighbours) (getBoardValues board))
    (every (function validNeighbours) (transpose (getBoardValues board)))
    (isDrecreaseByColumnGroup board)
  )
)

(defun isDrecreaseByColumnGroup (board)
  "Retorna se toda célula do tabuleiro respeita a regra de ter seu valor em ordem decrescente,
  caso o célula abaixo pertença ao mesmo grupo"
  (let ((lenBoard (len board)))
    (dotimes (n (* lenBoard (- lenBoard 1)))
      (let (
        (elemAbove (getElemByIdx board (+ n 1)))
        (elemUnder (getElemByIdx board (+ (+ n 1) lenBoard)))
      )
        (if (= (getElemId elemAbove) (getElemId elemUnder))
          (if
            (<
              (getElemValues (getElemValues elemAbove))
              (getElemValues (getElemValues elemUnder))
            )
            (return-from isDrecreaseByColumnGroup NIL)
          )
        )
      )
    )
    T
  )
)

(defun allSingle (board)
  "Retorna se todos as células do tabuleiro tem valor definido"
  (let
    ((filteredList (
      filter
      (function notNil)
      (myMap 
        (function (lambda (elems) (filter (function (lambda (el) (not (single el)))) elems)))
        (getBoardValues board)
      )
    )))
    (= (len filteredList) 0)
  )
)

(defun notNil (elem)
  "Retorna se o elemento é diferente de NIL"
  (not (equal elem NIL))
)

(defun cars (matrix)
  "Retorna uma lista com todos os elementos da primeira coluna de uma matriz"
  (if (null matrix)
    NIL
    (cons (car (car matrix)) (cars (cdr matrix)))
  )
)

(defun cdrs (matrix)
  "Retorna uma matriz sem sua primeira coluna"
  (if (null matrix)
    NIL
    (cons (cdr (car matrix)) (cdrs (cdr matrix)))
  )
)

; Retirado de https://stackoverflow.com/questions/39943232/matrix-transpose-common-lisp
(defun transpose (matrix)
  "Transpõe matriz"
  (cond ((null matrix) NIL)
        ((null (car matrix)) NIL)
        (t (cons (cars matrix) (transpose (cdrs matrix))))
  )
)

(defun firstWithPossibles (board)
  "Retorna a primeira célula do tabuleiro que ainda precisa definir um valor"
  (loop for line in board
    do (loop for elem in line
      do (if (not (single (getElemValues elem)))
        (return-from firstWithPossibles elem)  
      )
    )
  )
)

(defun searchSolution (board)
  "Procura o primeiro tabuleiro gerado que é uma solução válida para o tabuleiro passado como entrada"
  (cond
    ((allSingle board) (if (valid board) board NIL))
    (t
      (let ((elem (firstWithPossibles board)))
        (loop for choice in (getElemValues elem)
          do (let ((newElem (list (list choice) (getElemId elem) (getElemIdx elem))))
            (let (
              (newBoard
                (matrixMap 
                  (function (lambda (el all) (if (isEqual el elem) newElem el)))
                  board
                )
              ))
              (let ((result (searchSolution (possibleChoices newBoard))))
                (when result (return-from searchSolution result))
              )
            )
          )
        )
      )
    )
  )
)

(defun solver (board)
  "Inicia o processo de solução do tabuleiro"
  (searchSolution (possibleChoices (indexBoard board)))
)

(defun formatBoard (board)
  "Formata o tabuleiro, retirando os índices das células, para printar"
  (matrixMap (function (lambda (el all)
    (list (getElemValues el) (getElemId el))
  )) board)
)

(defun printMap (matrix)
  "Função auxiliar para printar uma matriz na tela"
  (if (null matrix)
    ()
    (progn
      (write-line (write-to-string (first matrix)))
      (printMap (rest matrix))
    )
  )
)

(write-line "Solucao do tabuleiro 6x6")
(printMap (formatBoard (solver (board6))))
;; Tempo indeterminado para solução, no mínimo mais de 10 minutos
;; (write-line "Solucao do tabuleiro 8x8")
;; (printMap (formatBoard (solver (board8))))