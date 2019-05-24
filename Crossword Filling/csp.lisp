;; ====================================================
;;  CMPU-365, Spring 2019, Dylan Horowitz
;;  Constraint Satisfaction Problem
;; ==================================================== 


;;  CSP-Node struct -- node in CSP tree
;; ----------------------------------------------------
;;  KEY: hash-table key
;;  DICTIONARY: list of words that could fill node
;;  CROSSING-WORDS: constraining variables
;;  COORDINATES: Coordinates of each character in node
;;  FILLED-WORD: Current solution for node
;;  EMPTY?: is the node filled?
;;  INVALID: list of words that have been determined 
;;      not to be the solution

(defstruct csp-node
  ;; The starting and ending coordinates of this node
  key
  ;; List of words that could fill this variable
  dictionary
  ;; Crossing words key: coordinate where two variables cross
  ;;                value: the variable this node crosses with
  (crossing-words (make-hash-table :test #'equal))
  ;; List of this node's coordinates
  coordinates
  ;; Whatever word is currently in this variable
  filled-word
  ;; Is this node filled?
  (empty? t)
  ;; Has this node gone through backtrack?
  (backtrack? nil)
  )

;;  CSP-TREE
;; ---------------------------------------------------------
;;  HASHY: hash-table whose entries are (key, value), where
;;          key = start and end coordinates of node
;;          value = mc-node
;;  ROOT-KEY: hash-table key for root node of csp tree

(defstruct csp-tree
  (hashy (make-hash-table :test #'equal))
  root-key)

;;  GET-ROOT-NODE
;; -----------------------------------------------------------
;;  INPUT: TREE, a CSP-TREE struct
;;  OUTPUT: The CSP-NODE corresponding to the root of the tree

(defun get-root-node
    (tree)
  (gethash (csp-tree-root-key tree) (csp-tree-hashy tree)))

;;  NEW-CSP-TREE
;; ------------------------------------------------------
;;  INPUT:   ROOT, csp node to be root
;;  OUTPUT:  A new csp tree whose root state is ROOT
;;           with children based on ROOT's crossing nodes

(defun new-csp-tree
    (root)
  (let ((tree (make-csp-tree :root-key (csp-node-key root))))
    (maphash #'(lambda (key val)
		 (setf (gethash (csp-node-key val) (csp-tree-hashy tree)) val))
	     (csp-node-crossing-words root))
    tree))


;;  WORD-IS-VALID?
;; ------------------------------------------------------ 
;;  INPUT: GAME, crossword struct
;;         COOR, list of coordinates
;;         WORD, word choice
;;         ACC, accumulator
;;  OUTPUT: T if valid possibility

(defun word-is-valid?
    (game coor word acc)
  (cond
   ((null coor) t)
   ((equal (nth (nth 1 (first coor)) (nth (nth 0 (first coor)) (crossword-grid game)))
	   1)
    (incf acc)
    (word-is-valid? game (rest coor) word acc))
   (t
    (cond
     ((equal (nth (nth 1 (first coor)) (nth (nth 0 (first coor)) (crossword-grid game)))
	     (char word acc))
      (incf acc)
      (word-is-valid? game (rest coor) word acc))
     (t
      nil)))
   )
  )

;;  COORDINATE-POSITION
;; ----------------------------------------------------------- 
;;  INPUT: COOR, coordinate
;;         LOS, list of coordinates
;;  OUTPUT: Position of COOR in LOS if present, NIL otherwise

(defun coordinate-position
    (coor los)
  (let ((position 0))
    (dolist (l los)
      (cond
       ((equal coor l)
	(return-from coordinate-position position))
       (t
	(incf position)))
      )
    )
  nil
  )

;;  VARIABLE-POSITION
;; -----------------------------------------------------------
;;  INPUT: VAR, csp-node struct
;;         LOV, list of csp-node structs
;;  OUTPUT: Position of VAR in LOV if present, NIL otherwise

(defun variable-position
    (var lov)
  (let ((position 0))
    (dolist (l lov)
      (cond
       ((equal VAR l)
        (return-from variable-position position))
       (t
        (incf position)))
      )
    )
  nil
  )

;;  NUMBER-WORD-OPTIONS
;; ------------------------------------------------------ 
;;  INPUT: CH, a character
;;         COORDINATE, coordinate where character would be
;;         VAR, variable to check
;;         GAME, a crossword struct
;;         START, a start point
;;         ACC, difference between start and end points
;;  OUTPUT: Number of words that this character allows to 
;;  be used when in the specific coordinate

(defun number-word-options
    (ch coordinate var game start acc) 
  (let* ((sum 0)
	(end (cond ((> (length (csp-node-dictionary var)) (+ start acc)) (+ start acc))
		   (t (- (length (csp-node-dictionary var)) 1))))
	 (diction (subseq (csp-node-dictionary var) start end)))
    (dolist (word diction)
      (cond
       ((word-is-valid? game (csp-node-coordinates var) word 0)
       (cond 
	((char= ch 
		(char word (coordinate-position coordinate (csp-node-coordinates var))))
	 (incf sum))))
       )
      )
    (cond
     ((and (= sum 0)
	   (< end (- (length (csp-node-dictionary var)) 1)))
      (number-word-options ch coordinate var game (+ start acc) acc)))
    sum))

;;  USED-WORD?
;; ------------------------------------------------------ 
;;  INPUT: GAME, a crossword struct
;;         WORD, a string
;;  OUTPUT: T if WORD is in USED-WORDS

(defun used-word?
    (game word)
  (dolist (w (crossword-used-words game))
    (cond
     ((equal w word) 
      (return-from used-word? t))
     )))

;;  SELECT-WORD
;; ------------------------------------------------------ 
;;  INPUT: GAME, a crossword struct
;;         VAR, a csp-node struct
;;         START, where in the dictionary to start the wordlist
;;         SLOT, VAR location in list of lines
;;  OUTPUT: VAR, updated with a filled word

(defun select-word
    (game var start)
  (let* ((g game)
	 (v var)
	 (*min-l* (cond ((> (length (csp-node-dictionary var)) (+ start *min-look*))
			 (+ start *min-look*))
			(t 
			 (length (csp-node-dictionary var)))))
	(wordlist (subseq (csp-node-dictionary var) start *min-l*))
	(maxWord nil)
	(maxWordProd 0)
	(currentWordProd 1))
    (dolist (word wordlist)
      (cond
       ((and (word-is-valid? g (csp-node-coordinates var) word 0)
	     (not (used-word? g word)))
	(maphash #'(lambda (key value)
		     (cond
		      ((not (csp-node-filled-word value))
		       (setf currentWordProd 
			 (* currentWordProd 
			    (number-word-options 
			     (char word (coordinate-position key (csp-node-coordinates var)))
			     key
			     value
			     game
			     0
			     100))))
		      ))
		 (csp-node-crossing-words var))
	(cond
	 ((or (> currentWordProd maxWordProd)
	      (and (= currentWordProd maxWordProd)
		   (= (random 2) 1)))
	  (setf maxWord word)
	  (setf maxWordProd currentWordProd))
	 )))
	(setf currentWordProd 1)
      )
    (cond
     (maxWord
      (setf (csp-node-empty? v) nil)
      (setf (csp-node-filled-word v) maxWord))
     ((not (>= (+ *min-look* start) (length (csp-node-dictionary v))))
      )
     (t
      (setf (csp-node-backtrack? var) t)
      (backtrack game var)
      (setf v (select-word game var 0))
      )
     )
  v)
)

;;  UNDO-MOVE
;; ------------------------------------------------------ 
;;  INPUT: GAME, a crossword struct
;;         VAR, a csp-node
;;  OUTPUT: GAME, updated without the filled-word of VAR
(defun undo-move
    (game var)
  (dolist (n (csp-node-coordinates var))
    (setf
	(nth (nth 1 n)
	     (nth (nth 0 n) (crossword-grid game)))
      1)))


;;  REDO-MOVE
;; ------------------------------------------------------ 
;;  INPUT: GAME, a crossword struct
;;         VAR, a csp-node with filled-word != NULL
;;  OUTPUT: GAME, updated with filled-word of VAR
(defun redo-move
    (game var)
  (let ((i 0))
    (dolist (n (csp-node-coordinates var))
      (setf 
	  (nth (nth 1 n)
	       (nth (nth 0 n) (crossword-grid game)))
	(char (csp-node-filled-word var) i))
      (incf i)
      ))
  )
    

;;  BACKTRACK
;; ------------------------------------------------------ 
;;  INPUT: GAME, a crossword struct
;;         VAR, a csp-node struct that is having problems
;;  OUTPUT: VAR, updated with filled-word
;;  SIDE-EFFECT, crossing-words variables updated to allow
;;      for a solution to VAR
(defun backtrack
    (game var)
  (let ((v var)
	(g game)
	(probWord nil)
	(temp nil)
	)
    (maphash #'(lambda (key val)
		 (cond
		  ((csp-node-filled-word val)
		   (setf probWord (csp-node-filled-word val))
		   (setf temp val)
		   (undo-move game temp)
		   (setf (csp-node-filled-word temp) nil)
		   (setf (csp-node-empty? temp) t)
		   (setf temp (backtrack-select g temp v 0 
						(char probWord (coordinate-position key (csp-node-coordinates temp)))
						(coordinate-position key (csp-node-coordinates temp))))
		   (cond
		    ((null (csp-node-filled-word temp))
		     (setf (csp-node-filled-word val) probWord)
		     (redo-move game val))
		    (t
		     (setf (nth (variable-position val (crossword-lines g)) (crossword-lines g)) temp)
		     (remove probWord (crossword-used-words g) :test #'string=)
		     (update-variables (crossword-lines game))
		     (update-grid (crossword-grid game) temp)
		     (setf (crossword-used-words game) (append (crossword-used-words game) (list (csp-node-filled-word temp))))
		     (print-crossword game t)
		     (return-from backtrack nil)))
		   )))
		  (csp-node-crossing-words var))
    )
    )

;;  BACKTRACK-SELECT
;; -------------------------------------------------------------
;;  INPUT: GAME, a crossword struct
;;         VAR, a csp-node struct
;;         ORIGIN, original problem csp-node struct
;;         START, where in the dictionary to start wordlist
;;  OUTPUT: VAR, updated with a filled word that's not PROBWORD

(defun backtrack-select
    (game var origin start probChar probPos)
  (let* ((g game)
	 (v var)
	 (*min-l* (cond ((> (length (member (csp-node-filled-word origin) 
					    (csp-node-dictionary var) :test #'string=)) 
			    (+ start *min-look*))
			 (+ start *min-look*))
			(t 
			 (length (member (csp-node-filled-word origin)
					    (csp-node-dictionary var) :test #'string=))
			    )))
	 (wordlist (subseq (member (csp-node-filled-word origin) 
				   (csp-node-dictionary var) :test #'string=) 
			   start *min-l*))
	 (maxWord nil)
	 (maxWordProd 0)
	 (currentWordProd 1))
    (dolist (word wordlist)
      (cond
       ((and (and (word-is-valid? g (csp-node-coordinates var) word 0)
		  (not (used-word? g word)))
	     (not (equal probChar (char word probPos)))) 
	(maphash #'(lambda (key value)
		     (cond
		      ((not (csp-node-filled-word value))
		       (setf currentWordProd
			 (* currentWordProd
			    (number-word-options
			     (char word (coordinate-position key (csp-node-coordinates var)))
			     key 
			     value
			     game
			     0
			     100))))
		      ))
		 (csp-node-crossing-words var))
	(cond
	 ((or (> currentWordProd maxWordProd)
	      (and (= currentWordProd maxWordProd)
		   (= (random 2) 1)))
	  (setf maxWord word)
	  (setf maxWordProd currentWordProd))
	 )))
      (setf currentWordProd 1)
      )
    (cond
     (maxWord
      (setf (csp-node-empty? v) nil)
      (setf (csp-node-filled-word v) maxWord))
     ((not (>= (+ *min-look* start) (length (csp-node-dictionary v))))
      (select-word game v (+ *min-look* start)))
     )
    v)
  )

;;  FILL-CROSSWORD
;; ------------------------------------------------------ 
;;  INPUT: GAME, a crossword struct
;;  OUTPUT: GAME, destructively updated with all
;;  variables filled

(defun fill-crossword
    (game)
  (let ((g game)
	(i 0)
	(start 0))
    (dolist (w (crossword-lines g))
      (loop do
	    (setf w (select-word g w start))
	    (cond
	     ((csp-node-backtrack? w)
	      (setf start 0)
	      (setf (csp-node-backtrack? w) nil)))
	    (setf start (+ *min-look* start))
	  while (null (csp-node-filled-word w)))
      (setf start 0)
      (setf w (select-word g w start))
      (setf (nth i (crossword-lines g)) w)
      (incf i)
      (update-variables (crossword-lines g))
      (update-grid (crossword-grid g) w)
      (setf (crossword-used-words g) (append (crossword-used-words g) (list (csp-node-filled-word w))))
      (print-crossword g t)
      )
    ))