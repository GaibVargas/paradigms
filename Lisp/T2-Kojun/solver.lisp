(defun board ()
  '(
    (((0) 0) ((0) 1) ((4) 1) ((0) 1) ((2) 3) ((0) 4))
    (((0) 0) ((0) 2) ((3) 1) ((0) 3) ((0) 3) ((0) 3))
    (((1) 0) ((4) 0) ((0) 5) ((4) 3) ((0) 10) ((0) 10)) 
    (((0) 6) ((5) 7) ((0) 5) ((0) 9) ((0) 9) ((2) 10)) 
    (((0) 6) ((0) 7) ((0) 7) ((0) 8) ((3) 8) ((0) 10)) 
    (((6) 7) ((2) 7) ((0) 7) ((2) 8) ((0) 8) ((5) 8))
  )
)

(defun indexBoard (board)
  (let ((aux '()))
    (matrixMap (function (lambda (el all)
      (append el (list (len (push 0 aux))))
    )) board)
  )
)

(defun len (list)
  (if (null list)
    0
    (+ 1 (len (rest list)))
  )
)

(defun single (list)
  (= (len list) 1)
)

(defun filter (f list)
  (if (null list)
    ()
    (if (funcall f (first list))
      (cons (first list) (filter f (rest list)))
      (filter f (rest list))
    )
  )
)

(defun myMap (f list &optional all)
  (if (null list)
    ()
    (if all
      (cons (funcall f (first list) all) (myMap f (rest list) all))
      (cons (funcall f (first list)) (myMap f (rest list)))
    )
  )
)

(defun matrixMap (f list &optional all)
  (if (null list)
    ()
    (if all
      (cons (myMap f (first list) all) (matrixMap f (rest list) all))
      (cons (myMap f (first list) list) (matrixMap f (rest list) list))    
    )
  )
)

(defun getElemId (elem)
  (nth 1 elem)
)

(defun getElemValues (elem)
  (nth 0 elem)
)

(defun getElemIdx (elem)
  (nth 2 elem)
)

(defun getBoardValues (board)
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
  (let ((merged (flatMatrix board)))
    (filterById groupId merged)
  )
)

(defun valuesInGroup (board groupId)
  (let ((filtered (filterByGroupId board groupId)))
    (let ((elemValues (myMap (function getElemValues) filtered)))
      (myMap (function getElemValues) (filter (function single) elemValues))
    )
  )
)

(defun fillList (n)
  (filling 1 n)
)

(defun filling (start stop)
  (if (> start stop)
    ()
    (cons start (filling (+ start 1) stop))
  )
)

(defun defineChoices (elem board)
  (if (= (getElemValues (getElemValues elem)) 0)
    (let
      (
        (initialList (fillList (len (filterByGroupId board (getElemId elem)))))
        (existingValues (valuesInGroup board (getElemId elem)))
      )
      (list (difference initialList existingValues) (getElemId elem) (getElemIdx elem))
    )
    (list 
      (difference (getElemValues elem) (valuesInGroup board (getElemId elem)))
      (getElemId elem)
      (getElemIdx elem)
    )
  )
)

(defun difference (list1 list2)
  (if (= (len list1) 1)
    list1
    (filter (lambda (elem) (not (member elem list2))) list1)
  )
)

(defun possibleChoices (board)
  (matrixMap (function defineChoices) board)
)

(defun generateNewBoards (elem board)
  (loop for choice in (getElemValues elem)
    do (write-line (write-to-string choice))
  )
)

(defun generateBoard (elem newElem board)
  (matrixMap (function ))
)

(defun isEqual (elem1 elem2)
  (= (getElemIdx elem1) (getElemIdx elem2))
)

(defun sameArray (arr1 arr2)
  (= (len (difference arr1 arr2)) 0)
)

(defun validNeighbours (list)
  (if (single list)
    T
    (let (
      (a (first list))
      (b (first (rest list)))
      (c (rest (rest list)))
      )
      (if (and (single (list a)) (single (list b)))
        (if (= a b)
          NIL
          (validNeighbours (cons b c))
        )
        (validNeighbours (cons b c))
      )
    )
  )
)

(defun decrease (list)
  (if (single list)
    T
    (let (
      (a (first list))
      (b (first (rest list)))
      (c (rest (rest list)))
      )
      (if (and (single (list a)) (single (list b)))
        (if (< a b)
          NIL
          (decrease (cons b c))
        )
        (decrease (cons b c))
      )
    )
  )
)

(defun valid (board)
  (and
    (every (function validNeighbours) (getBoardValues board))
    (every (function validNeighbours) (transpose (getBoardValues board)))
  )
)

(defun cars (matrix)
  "Return a list with all the cars of the lists in matrix"
  (if (null matrix)
    NIL
    (cons (car (car matrix)) (cars (cdr matrix)))
  )
)

(defun cdrs (matrix)
  "Return a list with all the cdrs of the lists in matrix"
  (if (null matrix)
    NIL
    (cons (cdr (car matrix)) (cdrs (cdr matrix)))
  )
)

; Retirado de https://stackoverflow.com/questions/39943232/matrix-transpose-common-lisp
(defun transpose (matrix)
  "Transpose matrix"
  (cond ((null matrix) NIL)
        ((null (car matrix)) NIL)
        (t (cons (cars matrix) (transpose (cdrs matrix))))
  )
)

(defun searchSolution (board)
  (if (valid stop)
    board
    (loop for line in board
      do (loop for elem in line
        do (if (not (single (getElemValues elem)))
          (loop for choice in (getElemValues elem)
            do (let ((newElem (list (list choice) (getElemId elem) (getElemIdx elem))))
              (let 
                (
                  (newBoard (matrixMap (function (lambda (el all)
                    (if (isEqual el elem)
                      newElem
                      el
                    )))
                    board)
                  )
                )
                (return-from searchSolution (searchSolution (possibleChoices newBoard)))
              )
            )
          )
        )
      )
    )
  )
)

(defun formatBoard (board)
  (matrixMap (function (lambda (el all)
    (list (getElemValues el) (getElemId el))
  )) board)
)

(defun printMap (matrix)
  (if (null matrix)
    ()
    (progn
      (write-line (write-to-string (first matrix)))
      (printMap (rest matrix))
    )
  )
)

;; (printMap (formatBoard (searchSolution (possibleChoices (indexBoard (board))))))
;; (write (validNeighbours '(1 2 3 4 4)))
;; (write (transpose '((1 2 3) (4 5 6))))
(write (valid '(((1) (2)) ((1) (2)))))
(write (decrease '(5 3 1)))