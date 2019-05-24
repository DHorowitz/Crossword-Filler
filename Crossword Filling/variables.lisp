;; ====================================================
;;  CMPU-365, Spring 2019, Dylan Horowitz
;;  Constraint Satisfaction Problem Crossword Solver
;;  tests.lisp = Test Parameters
;; ====================================================

;;  5 x 5 grid Crosswords - A
;; ----------------------------------------------------

(defparameter *A1* (list (list 0 0 0 0 1)
			 (list 0 1 1 1 1)
			 (list 1 1 1 0 1)
			 (list 0 1 1 0 1)
			 (list 0 1 0 0 0)))

(defparameter *A2* (list (list 0 0 1 1 1)
			 (list 0 1 1 1 1)
			 (list 1 1 1 1 1)
			 (list 1 1 1 1 0)
			 (list 1 1 1 0 0)))

(defparameter *A3* (list (list 1 1 1 1 1)
			 (list 1 0 1 0 1)
			 (list 1 1 1 1 1)
			 (list 1 0 1 0 1)
			 (list 1 1 1 1 1)))

;;  8 x 8 grid Crosswords - B
;; ---------------------------------------------------- 

(defparameter *B1* (list (list 0 0 1 0 0 0 0 1)
			 (list 1 1 1 1 1 1 1 1)
			 (list 1 0 1 0 1 0 0 1)
			 (list 1 0 1 0 1 0 0 0)
			 (list 1 0 0 0 1 0 0 1)
			 (list 1 0 0 1 1 1 1 1)
			 (list 1 0 0 0 1 0 0 1)
			 (list 1 1 1 1 0 0 0 1)))

(defparameter *B2* (list (list 0 0 1 1 1 1 1 1)
			 (list 0 0 0 0 1 0 0 1)
			 (list 1 0 1 1 1 1 0 1)
			 (list 1 0 0 0 1 0 0 1)
			 (list 1 0 1 1 1 1 0 0)
			 (list 1 0 0 0 1 0 1 0)
			 (list 1 1 1 1 1 1 1 0)
			 (list 1 0 0 0 1 0 1 0)))

(defparameter *B3* (list (list 0 0 0 1 1 0 0 0)
			 (list 0 0 1 1 1 1 0 0)
			 (list 0 1 1 1 1 1 1 0)
			 (list 1 1 1 0 0 1 1 1)
			 (list 1 1 1 0 0 1 1 1)
			 (list 0 1 1 1 1 1 1 0)
			 (list 0 0 1 1 1 1 0 0)
			 (list 0 0 0 1 1 0 0 0)))

;;  10 x 10 grid Crosswords - C
;; ---------------------------------------------------

(defparameter *C1* (list (list 0 1 1 1 1 0 1 0 1 0)
			 (list 1 0 1 0 1 1 1 1 1 1)
			 (list 1 1 1 1 1 0 1 0 1 0)
			 (list 1 0 1 0 1 1 1 1 1 1)
			 (list 1 1 1 1 1 1 1 0 1 0)
			 (list 0 1 0 1 1 1 1 1 1 1)
			 (list 1 1 1 1 1 1 0 1 0 1)
			 (list 0 1 0 1 0 1 1 1 1 1)
			 (list 1 1 1 1 1 1 0 1 0 1)
			 (list 0 1 0 1 0 1 1 1 1 0)))

;;  Variables for Crossword Group A
;; ----------------------------------------------

(defparameter *A1Variables* 
    (list (make-csp-node :coordinates (list (list 2 0) (list 2 1) (list 2 2))
			 :key (list (list 2 0) (list 2 2)))	  
	  (make-csp-node :coordinates (list (list 1 1) (list 2 1) (list 3 1) (list 4 1))
			 :key (list (list 1 1) (list 4 1)))
	  (make-csp-node :coordinates (list (list 1 2) (list 2 2) (list 3 2))
			 :key (list (list 1 2) (list 3 2)))
	  (make-csp-node :coordinates (list (list 3 1) (list 3 2))
			 :key (list (list 3 1) (list 3 2)))
	  (make-csp-node :coordinates (list (list 1 1) (list 1 2) (list 1 3) (list 1 4))
			 :key (list (list 1 1) (list 1 4)))	  
	  (make-csp-node :coordinates (list (list 0 4) (list 1 4) (list 2 4) (list 3 4))
			 :key (list (list 0 4) (list 3 4)))
	  ))

(defparameter *A2Variables*
    (list (make-csp-node :coordinates (list (list 0 4) (list 1 4) (list 2 4))
			 :key (list (list 0 4) (list 2 4)))
	  (make-csp-node :coordinates (list (list 0 3) (list 1 3) (list 2 3) (list 3 3))
			 :key (list (list 0 3) (list 3 3)))
	  (make-csp-node :coordinates (list (list 0 2) (list 1 2) (list 2 2) (list 3 2) (list 4 2))
			 :key (list (list 0 2) (list 4 2)))
	  (make-csp-node :coordinates (list (list 1 1) (list 2 1) (list 3 1) (list 4 1))
			 :key (list (list 1 1) (list 4 1)))
	  (make-csp-node :coordinates (list (list 2 0) (list 3 0) (list 4 0))
			 :key (list (list 2 0) (list 4 0)))
	  (make-csp-node :coordinates (list (list 0 2) (list 0 3) (list 0 4))
			 :key (list (list 0 2) (list 0 4)))
	  (make-csp-node :coordinates (list (list 1 1) (list 1 2) (list 1 3) (list 1 4))
			 :key (list (list 1 1) (list 1 4)))
	  (make-csp-node :coordinates (list (list 2 0) (list 2 1) (list 2 2) (list 2 3) (list 2 4))
			 :key (list (list 2 0) (list 2 4)))
	  (make-csp-node :coordinates (list (list 3 0) (list 3 1) (list 3 2) (list 3 3))
			 :key (list (list 3 0) (list 3 3)))
	  (make-csp-node :coordinates (list (list 4 0) (list 4 1) (list 4 2))
			 :key (list (list 4 0) (list 4 2)))
	  ))

(defparameter *A3Variables*
    (list (make-csp-node :coordinates (list (list 0 0) (list 1 0) (list 2 0) (list 3 0) (list 4 0))
			 :key (list (list 0 0) (list 4 0)))
	  (make-csp-node :coordinates (list (list 0 2) (list 1 2) (list 2 2) (list 3 2) (list 4 2))
			 :key (list (list 0 2) (list 4 2)))
	  (make-csp-node :coordinates (list (list 0 4) (list 1 4) (list 2 4) (list 3 4) (list 4 4))
			 :key (list (list 0 4) (list 4 4)))
	  (make-csp-node :coordinates (list (list 0 0) (list 0 1) (list 0 2) (list 0 3) (list 0 4))
			 :key (list (list 0 0) (list 0 4)))
	  (make-csp-node :coordinates (list (list 2 0) (list 2 1) (list 2 2) (list 2 3) (list 2 4))
			 :key (list (list 2 0) (list 2 4)))
	  (make-csp-node :coordinates (list (list 4 0) (list 4 1) (list 4 2) (list 4 3) (list 4 4))
			 :key (list (list 4 0) (list 4 4)))
	  ))

;;  Variables for crossword group B
;; ------------------------------------------------------------

(defparameter *B1Variables*
    (list (make-csp-node :coordinates (list (list 0 2) (list 1 2) (list 2 2) (list 3 2))
			 :key (list (list 0 2) (list 3 2)))
	  (make-csp-node :coordinates (list (list 1 0) (list 2 0) (list 3 0) (list 4 0)
					    (list 5 0) (list 6 0) (list 7 0))
			 :key (list (list 1 0) (list 7 0)))
	  (make-csp-node :coordinates (list (list 1 4) (list 2 4) (list 3 4) (list 4 4)
					    (list 5 4) (list 6 4))
			 :key (list (list 1 4) (list 6 4)))
	  (make-csp-node :coordinates (list (list 0 7) (list 1 7) (list 2 7))
			 :key (list (list 0 7) (list 2 7)))
	  (make-csp-node :coordinates (list (list 4 7) (list 5 7) (list 6 7) (list 7 7))
			 :key (list (list 4 7) (list 7 7)))
	  (make-csp-node :coordinates (list (list 1 0) (list 1 1) (list 1 2) (list 1 3) 
					    (list 1 4) (list 1 5) (list 1 6) (list 1 7))
			 :key (list (list 1 0) (list 1 7)))
	  (make-csp-node :coordinates (list (list 5 3) (list 5 4) (list 5 5) (list 5 6) 
					    (list 5 7))
			 :key (list (list 5 3) (list 5 7)))
	  (make-csp-node :coordinates (list (list 7 0) (list 7 1) (list 7 2) (list 7 3))
			 :key (list (list 7 0) (list 7 3)))
	  ))

(defparameter *B2Variables*
    (list (make-csp-node :coordinates (list (list 2 0) (list 3 0) (list 4 0) (list 5 0)
					    (list 6 0) (list 7 0))
			 :key (list (list 2 0) (list 7 0)))
	  (make-csp-node :coordinates (list (list 0 4) (list 1 4) (list 2 4) (list 3 4)
					    (list 4 4) (list 5 4) (list 6 4) (list 7 4))
			 :key (list (list 0 4) (list 7 4)))
	  (make-csp-node :coordinates (list (list 5 6) (list 6 6) (list 7 6))
			 :key (list (list 5 6) (list 7 6)))
	  (make-csp-node :coordinates (list (list 0 7) (list 1 7) (list 2 7) (list 3 7))
			 :key (list (list 0 7) (list 3 7)))
	  (make-csp-node :coordinates (list (list 0 2) (list 0 3) (list 0 4) (list 0 5)
					    (list 0 6) (list 0 7))
			 :key (list (list 0 2) (list 0 7)))
	  (make-csp-node :coordinates (list (list 2 2) (list 2 3) (list 2 4) (list 2 5))
			 :key (list (list 2 2) (list 2 5)))
	  (make-csp-node :coordinates (list (list 4 2) (list 4 3) (list 4 4) (list 4 5))
			 :key (list (list 4 2) (list 4 5)))
	  (make-csp-node :coordinates (list (list 6 0) (list 6 1) (list 6 2) (list 6 3) 
					    (list 6 4) (list 6 5) (list 6 6))
			 :key (list (list 6 0) (list 6 6)))
	  ))

(defparameter *B3Variables*
    (list (make-csp-node :coordinates (list (list 3 0) (list 4 0))
			 :key (list (list 3 0) (list 4 0)))
	  (make-csp-node :coordinates (list (list 2 1) (list 3 1) (list 4 1) (list 5 1))
			 :key (list (list 2 1) (list 5 1)))
	  (make-csp-node :coordinates (list (list 1 2) (list 2 2) (list 3 2) (list 4 2)
					    (list 5 2) (list 6 2))
			 :key (list (list 1 2) (list 6 2)))
	  (make-csp-node :coordinates (list (list 0 3) (list 1 3) (list 2 3))
			 :key (list (list 0 3) (list 2 3)))
	  (make-csp-node :coordinates (list (list 5 3) (list 6 3) (list 7 3))
			 :key (list (list 5 3) (list 7 3)))
	  (make-csp-node :coordinates (list (list 0 4) (list 1 4) (list 2 4))
			 :key (list (list 0 4) (list 2 4)))
	  (make-csp-node :coordinates (list (list 5 4) (list 6 4) (list 7 4))
			 :key (list (list 5 4) (list 7 4)))
	  (make-csp-node :coordinates (list (list 1 5) (list 2 5) (list 3 5) (list 4 5)
					    (list 5 5) (list 6 5))
			 :key (list (list 1 5) (list 6 5)))
	  (make-csp-node :coordinates (list (list 2 6) (list 3 6) (list 4 6) (list 5 6))
			 :key (list (list 2 6) (list 5 6)))
	  (make-csp-node :coordinates (list (list 3 7) (list 4 7))
			 :key (list (list 3 7) (list 4 7)))
	  (make-csp-node :coordinates (list (list 0 3) (list 0 4))
			 :key (list (list 0 3) (list 0 4)))
	  (make-csp-node :coordinates (list (list 1 2) (list 1 3) (list 1 4) (list 1 5))
			 :key (list (list 1 2) (list 1 5)))
	  (make-csp-node :coordinates (list (list 2 1) (list 2 2) (list 2 3) (list 2 4)
					    (list 2 5) (list 2 6))
			 :key (list (list 2 1) (list 2 6)))
	  (make-csp-node :coordinates (list (list 3 0) (list 3 1) (list 3 2))
			 :key (list (list 3 0) (list 3 2)))
	  (make-csp-node :coordinates (list (list 3 5) (list 3 6) (list 3 7))
			 :key (list (list 3 5) (list 3 7)))
	  (make-csp-node :coordinates (list (list 4 0) (list 4 1) (list 4 2))
			 :key (list (list 4 0) (list 4 2)))
	  (make-csp-node :coordinates (list (list 4 5) (list 4 6) (list 4 7))
			 :key (list (list 4 5) (list 4 7)))
	  (make-csp-node :coordinates (list (list 5 1) (list 5 2) (list 5 3) (list 5 4)
					    (list 5 5) (list 5 6))
			 :key (list (list 5 1) (list 5 6)))
	  (make-csp-node :coordinates (list (list 6 2) (list 6 3) (list 6 4) (list 6 5))
			 :key (list (list 6 2) (list 6 5)))
	  (make-csp-node :coordinates (list (list 7 3) (list 7 4))
			 :key (list (list 7 3) (list 7 4)))
	  ))

;;  Variables for crossword group C
;; -------------------------------------------------------------------------d

(defparameter *C1Variables*
    (list (make-csp-node :coordinates (list (list 1 0) (list 2 0) (list 3 0) (list 4 0))
			 :key (list (list 1 0) (list 4 0)))
	  (make-csp-node :coordinates (list (list 4 1) (list 5 1) (list 6 1) (list 7 1)
					    (list 8 1) (list 9 1))
			 :key (list (list 4 1) (list 9 1)))
	  (make-csp-node :coordinates (list (list 0 2) (list 1 2) (list 2 2) (list 3 2)
					    (list 4 2))
			 :key (list (list 0 2) (list 4 2)))
	  (make-csp-node :coordinates (list (list 4 3) (list 5 3) (list 6 3) (list 7 3)
					    (list 8 3) (list 9 3))
			 :key (list (list 4 3) (list 9 3)))
	  (make-csp-node :coordinates (list (list 0 4) (list 1 4) (list 2 4) (list 3 4)
					    (list 4 4) (list 5 4) (list 6 4))
			 :key (list (list 0 4) (list 6 4)))
	  (make-csp-node :coordinates (list (list 3 5) (list 4 5) (list 5 5) (list 6 5)
					    (list 7 5) (list 8 5) (list 9 5))
			 :key (list (list 3 5) (list 9 5)))
	  (make-csp-node :coordinates (list (list 0 6) (list 1 6) (list 2 6) (list 3 6)
					    (list 4 6) (list 5 6))
			 :key (list (list 0 6) (list 5 6)))
	  (make-csp-node :coordinates (list (list 5 7) (list 6 7) (list 7 7) (list 8 7) 
					    (list 9 7))
			 :key (list (list 5 7) (list 9 7)))
	  (make-csp-node :coordinates (list (list 0 8) (list 1 8) (list 2 8) (list 3 8)
					    (list 4 8) (list 5 8))
			 :key (list (list 0 8) (list 5 8)))
	  (make-csp-node :coordinates (list (list 5 9) (list 6 9) (list 7 9) (list 8 9))
			 :key (list (list 5 9) (list 8 9)))
	  (make-csp-node :coordinates (list (list 0 1) (list 0 2) (list 0 3) (list 0 4))
			 :key (list (list 0 1) (list 0 4)))
	  (make-csp-node :coordinates (list (list 1 4) (list 1 5) (list 1 6) (list 1 7)
					    (list 1 8) (list 1 9))
			 :key (list (list 1 4) (list 1 9)))
	  (make-csp-node :coordinates (list (list 2 0) (list 2 1) (list 2 2) (list 2 3) 
					    (list 2 4))
			 :key (list (list 2 0) (list 2 4)))
	  (make-csp-node :coordinates (list (list 3 4) (list 3 5) (list 3 6) (list 3 7) 
					    (list 3 8) (list 3 9))
			 :key (list (list 3 4) (list 3 9)))
	  (make-csp-node :coordinates (list (list 4 0) (list 4 1) (list 4 2) (list 4 3)
					    (list 4 4) (list 4 5) (list 4 6))
			 :key (list (list 4 0) (list 4 6)))
	  (make-csp-node :coordinates (list (list 5 3) (list 5 4) (list 5 5) (list 5 6)
					    (list 5 7) (list 5 8) (list 5 9))
			 :key (list (list 5 3) (list 5 9)))
	  (make-csp-node :coordinates (list (list 6 0) (list 6 1) (list 6 2) (list 6 3)
					    (list 6 4) (list 6 5))
			 :key (list (list 6 0) (list 6 5)))
	  (make-csp-node :coordinates (list (list 7 5) (list 7 6) (list 7 7) (list 7 8) 
					    (list 7 9))
			 :key (list (list 7 5) (list 7 9)))
	  (make-csp-node :coordinates (list (list 8 0) (list 8 1) (list 8 2) (list 8 3)
					    (list 8 4) (list 8 5))
			 :key (list (list 8 0) (list 8 5)))
	  (make-csp-node :coordinates (list (list 9 5) (list 9 6) (list 9 7) (list 9 8))
			 :key (list (list 9 5) (list 9 8)))
    ))


;;  Make Dictionary
;; ------------------------------------------

(defparameter wordlist
    (with-open-file (stream "words.lisp")
      (read stream)))

(defparameter dict (make-hash-table :test 'equal))

;;  ADD-WORDS-TO-DICT
;; ------------------------------------------
;;  INPUT: list of words
;;  OUTPUT: NIL
;;  SIDE-EFFECT: add list of words to dict

(defun add-words-to-dict (wlist)
  ;; For each word in the list...
  (dolist (word wlist)
    ;; ... place in dictionary based on length
    (setf 
	(gethash (length word) dict)
      (append (list word) (gethash (length word) dict)))
    ))
    
;;  MAKE-DICT
;; ----------------------------------------
;;  INPUT: N/A
;;  OUTPUT: N/A
;;  SIDE-EFFECT: Fill dict with dictionary
;;  from wordlist

(defun make-dict ()
  (add-words-to-dict wordlist)
  ;; Add-words-to-dict inserts the words in 
  ;; reverse alphabetical order, so use maphash
  ;; to reverse said order
  (maphash #'(lambda (key val)
	       (setf (gethash key dict) (reverse val)))
	   dict)
  )
