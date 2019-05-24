;; =========================================
;;  CMPU-365, Spring 2019
;;  Basic Definitions for Crossword Project
;; =========================================


;;  To ensure that the compiler efficiently handles tail recursion
(setq compiler:tail-call-self-merge-switch t)
(setq compiler:tail-call-non-self-merge-switch t)

;;  To avoid annoying garbage-collection messages
(setf *global-gc-behavior* :auto)

;;  min-look value
(defparameter *min-look* 10)



;;  List of files for the Crossword implementation:

(defparameter *crossword-files*
    (list "basics"
	  "crossword-starter"
	  "csp"
	  "variables"))

;;  MAKER
;; -----------------------------------------------
;;  Compiles and loads all files

(defun maker
    ()
  (dolist (file *crossword-files*)
    (compile-file file)
    (load file))
  (make-dict))
