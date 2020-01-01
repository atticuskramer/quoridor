;;; --------------------------------------------
;;;  CMPU-365, Spring 2017
;;;  FINAL PROJECT
;;;  Testing File for Macall McQueen and Atticus Kramer
;;; --------------------------------------------
;;;  To compile and load all files and run a few tests,
;;;  simply load this file:  (load "testing.lisp" :verbose nil)


;; A list of all the relevant files we'd like to compile. 

(defparameter *our-files* 
	(list 
		"a-star-basic-defns" 
		"a-star" 
		"quoridor" 
		"alpha-beta-mm"
		))


;;;  COMPILE-AND-LOAD
;;; ------------------------------
;;;  INPUT:  FILENAME, a string
;;;  OUTPUT:  None
;;;  SIDE EFFECT: Compiles-and-loads the file with the compiler
;;                flags set to optimize for tail recursion.
;;;  NOTE:  Do NOT include the ".lisp" extension in the filename;
;;;         otherwise it will load the UNcompiled file!!

(defun compile-and-load (filename)
  ;;  COMPILER-FLAGS
  (setq compiler:tail-call-self-merge-switch t)
  (setq compiler:tail-call-non-self-merge-switch t) 
  (compile-file filename :verbose nil)
  (load filename :verbose nil))


;;;  MAKER
;;; ------------------------------
;;;  INPUT:  LIST-O-FILES, a list of strings
;;;  OUTPUT:  T, if all files compiled and loaded successfully
;;;  SIDE EFFECT:  Compiles and loads all indicated files

(defun maker
    (list-o-files)
  (mapcar #'compile-and-load
	  list-o-files))


;; COMPILE and LOAD all of the relevant files

(maker *our-files*)


;;  COMPUTE-DO-AND-SHOW-N-MOVES
;; ------------------------------------------
;;  INPUTS:  G, a quoridor struct
;;           N, a positive integer
;;           CUTOFF-DEPTH, the cutoff depth for minimax
;;           EVAL-FUNC-BLK, EVAL-FUNC-WHT, the static evaluation
;;              functions that each player will be using. 
;;  SIDE EFFECT:  Computes, does, and shows the results of N
;;                moves generated using COMPUTE-MOVE.

(defun compute-do-and-show-n-moves
    (g n cutoff-depth eval-func-blk eval-func-wht)
  (let ((mv nil)
	(start-time (get-internal-run-time)))
    
    (dotimes (i n)
      (format t "~%~A~%" g)

      ;; We switch evaluation functions every time we switch players.
      (setf mv (compute-move g cutoff-depth 
      	(if (evenp i) eval-func-blk eval-func-wht)))

      (apply #'do-move! g nil mv)
  (format t "     BLACK SHORTEST PATH: ~A~%" (shortest-path g *black*))
  (format t "     WHITE SHORTEST PATH: ~A~%" (shortest-path g *white*))

      ;; If the game ends, we stop computing moves and announce the winner.
      (when (game-over? g)
      	(format t "~%~A~%" g)
	(format t "Time: ~A~%" (- (get-internal-run-time) start-time))
      	(return-from compute-do-and-show-n-moves
      		(format t "~A won!!!!~%" 
      		(if (= *black* (quoridor-whose-turn g))
      			"WHITE" "BLACK")))))

    (format t "Time: ~A~%" (- (get-internal-run-time) start-time))
    (format t "~%~A~%" g)))


(setq g (make-quoridor))
(compute-do-and-show-n-moves g 1000 4 #'eval-func-1 #'eval-func-1)
