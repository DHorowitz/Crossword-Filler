;; ====================================================
;;  CMPU-365, Spring 2019, Dylan Horowitz
;;  Constraint Satisfaction Problem Crossword Solver
;;  Crossword-starter.lisp
;; ====================================================

;;  The CROSSWORD struct
;; ----------------------------------------------------

(defstruct crossword
  ;; Current game state
  grid
  ;; However bit this grid is
  grid-size
  ;; List of this game's nodes
  lines
  ;; List of all words currently in crossword
  used-words
  )


;;  UPDATE-GRID
;; ---------------------------------------------------------
;;  INPUT: GR, a grid object
;;         VAR, a csp-node
;;  OUTPUT: GR updated with letters from csp-node

(defun update-grid
    (gr var)
  (cond
   ((equal (csp-node-filled-word var) nil)
    (return-from update-grid gr)))
  (let ((i 0))
    (dolist (n (csp-node-coordinates var))
      (setf 
	  (nth (nth 1 n) 
	       (nth (nth 0 n) gr))
	(char (csp-node-filled-word var) i))
      (incf i)
      ))
  )

;;  CROSS?
;; ---------------------------------------------------------
;;  INPUT: VARX, csp-node
;;         VARY, csp-node
;;  OUTPUT: coordinates where variables cross if they do,
;;          NIL otherwise

(defun cross?
    (varx vary)
  (dolist (i (csp-node-coordinates varx))
    (dolist (r (csp-node-coordinates vary))
      (cond
       ((equal i r)
	(return-from cross? i)))
  )))

;;  IS-VALID-CHOICE?
;; --------------------------------------------------------- 
;;  INPUT: LOW, a list of words
;;         WORD, the word to check
;;  OUTPUT: T if word isn't currently in use

(defun is-valid-choice?
    (low word)
  (cond
   ((null low) t)
   ((string= word (first low)) (return-from is-valid-choice? nil))
   (t
    (is-valid-choice? (rest low) word))
   ))

;;  GET-VARIABLE-LENGTH
;; ---------------------------------------------------------
;;  INPUT: v, variable
;;  OUTPUT: n, length of variable

(defun get-variable-length
    (v)
  (let ((n 0))
    (setf n (+ (abs 
		(- 
		 (nth 0 (nth 0 (csp-node-key v))) 
		 (nth 0 (nth 1 (csp-node-key v)))))
	       (abs 
		(- 
		 (nth 1 (nth 0 (csp-node-key v))) 
		 (nth 1 (nth 1 (csp-node-key v)))
		 ))
	       ))
    (incf n)
    n))

;;  PRINT-CROSSWORD
;; --------------------------------------------------------
;;  INPUT: G, crossword struct
;;         STR, output stream (or T)
;;  OUTPUT: N/A
;;  SIDE-EFFECT: print the grid into std out

(defun print-crossword
    (g str)
  (format str  "~%  | ")
  (dotimes (i (crossword-grid-size g))
    (format str "~A " i))
  (format str "~%")
  (format str "----------")
  (dotimes (j (crossword-grid-size g))
    (format str "-"))
  (dotimes (r (crossword-grid-size g))
    (format str "~% ~A| " r)
    (dotimes (c (crossword-grid-size g))
      (let ((token (nth r (nth c (crossword-grid g)))))
	(format str "~A " (cond ((equal token 0) '0)
				((equal token 1) '_)
				(t token))))))
  (format str "~%"))


;;  UPDATE-VARIABLES
;; ---------------------------------------------------------
;;  INPUT: LOV, list of variables
;;  OUTPUT: LOV, same list of variables
;;  SIDE-EFFECT: LOV updates with crossing-words and appropriate dictionary

(defun update-variables
    (lov)
  (dolist (n lov)
    (setf (csp-node-dictionary n) (gethash (get-variable-length n) dict))
    (dolist (v lov)
      (cond
       ((and 
	 (cross? n v)
	 (not
	  (equal n v)))
	(setf 
	    (gethash (cross? n v) (csp-node-crossing-words n)) 
	  v)))))
  lov)

;;  NEW-CROSSWORD
;; -------------------------------------------------------------
;;  INPUT: SELECTED-BOARD, which board to fill
;;         SELECTED-BOARD-LINES, list of variables for the board
;;  OUTPUT: CROSSWORD struct

(defun new-crossword
    (selected-board selected-board-lines)
  (let ((game (make-crossword :grid selected-board
			      :grid-size (length selected-board)
			      :lines (update-variables selected-board-lines))))
    (print-crossword game t)
    game))