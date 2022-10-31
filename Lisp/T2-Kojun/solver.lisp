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
      (list (difference initialList existingValues) (getElemId elem))
    )
    elem
  )
)

(defun difference (list1 list2)
  (filter (lambda (elem) (not (member elem list2))) list1)
)

(defun possibleChoices (board)
  (matrixMap (function defineChoices) board)
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

(printMap (possibleChoices (board)))