	  ;; ====================================
;;  CMPU-365, Spring 2017
;;  Asmt. 4
;;  Macall McQueen
;;  alpha-beta-mm.lisp
;;  Feb. 28, 2017
;; ====================================


;;  AB-NODE struct
;; ---------------------------
;;  Contains information about a particular node in an alpha-beta search tree

(defstruct ab-node
  depth
  val
  (exact nil))


;;  STATS struct
;; ---------------------------
;;  Stats compiled during minimax search

(defstruct stats
  (num-moves-done 0)
  (num-potential-moves 0))

;;  COMPUTE-MOVE
;; -------------------------------------------------------------
;;  INPUTS:  G, a CHESS struct
;;           CUTOFF-DEPTH, to limit depth of minimax search
;;  OUTPUT:  The best move according to MINIMAX with ALPHA-BETA
;;   pruning, using the static eval func, EVAL-FUNC.  Searches to
;;   a depth of CUTOFF-DEPTH.

(defun compute-move (g cutoff-depth eval-func)
  (format t "~%COMPUTE-MOVE (cutoff=~A)~%" cutoff-depth)

  (let* (
	 (statty (make-stats))
	 (hashy (make-hash-table :test #'equalp))

	 ;; Call compute-max 
	 (best-mv-info 
	  (compute-max g 0 *neg-inf* *pos-inf* statty cutoff-depth eval-func hashy))

	 ;; At depth 0, compute-max returns a list containing the best move
	 ;; it found and the root alpha value 
	 (best-move (first best-mv-info))
	 (root-alpha (second best-mv-info))

	 (moves-done (stats-num-moves-done statty))
	 (potential-moves (stats-num-potential-moves statty))
	 )

    (format t "     ROOT NODE ALPHA: ~A~%" root-alpha)
    (format t "     NUM-MOVES-DONE ~A, NUM-MOVES-PRUNED: ~A~%"
	    moves-done (- potential-moves moves-done))
    (format t "     BEST-MOVE: ~A~%" best-move)
    best-move))


;;  COMPUTE-MAX
;; ---------------------------------------------------------------
;;  INPUTS:  G, a CHESS struct
;;           CURR-DEPTH, the current depth in the search
;;           ALPHA, BETA, alpha/beta values for this node in search
;;           STATTY, stats struct
;;           CUTOFF-DEPTH, to limit depth of minimax search
;;           EVAL-FUNC, the evaluation function for G
;;           HASHY, a hash table representing the search tree
;;  OUTPUT:  If CURR-DEPTH is zero, returns best move
;;           Otherwise returns value of this node according
;;           to MINIMAX with ALPHA-BETA pruning.

(defun compute-max (g curr-depth alpha beta statty cutoff-depth eval-func hashy)
  (let* ((key (make-hash-key g))
	 (nodey (gethash key hashy)))
    (cond
     ;; If the game is over, indicate that we've lost
     ((game-over? g)
      (+ *loss-value* curr-depth))

     ;; If we've reached the cutoff-depth, we use the static evaluation
     ;; function to determine the current node's numerical value
     ((>= curr-depth cutoff-depth)
      (funcall eval-func g))
     
     ;; If the current state already has a node in the tree, the node is at a 
     ;; depth, less than the current, and it's alpha value is exact or greater
     ;; than the parent beta
     ((and nodey
	   (<= (ab-node-depth nodey) curr-depth)
	   (or (ab-node-exact nodey)
	       (>= (ab-node-val nodey) beta)))
      ;; Just return the value of the node already in the tree
      ;(format t "Max found an identical node. Node value: ~A, beta: ~A~%" (ab-node-val nodey) beta) 
      (ab-node-val nodey))
     

     ;; Otherwise,
     (t 
      ;; we use LEGAL-MOVES to fetch a list of legal moves. 
      (let* (
	     (moves (legal-moves g))
	     (best-move (first moves))
	     )

	;; Increment the NUM-POTENTIAL-MOVES field in the STATS data
	;; structure by the number of legal moves.
	(incf (stats-num-potential-moves statty) (length moves))

	;; For each move in that list, 
	(dolist (mv moves)

	  ;; increment the number of moves done
	  (incf (stats-num-moves-done statty) 1)

	  ;; do the move
	  (apply #'do-move! g nil mv)

	  ;; make a recursive call to COMPUTE-MIN
	  (let ((new-alpha
		 (compute-min g 
			      (+ 1 curr-depth) alpha beta statty cutoff-depth eval-func hashy)))

	    ;; undo the move
	    (undo-move! g)

	    ;; If the value returned by COMPUTE-MIN is greater than the previous
	    ;; alpha, we update alpha.
	    (when (> new-alpha alpha)
	      (setq alpha new-alpha)
	      (setq best-move mv)

	      ;; and see if pruning is possible.
	      (when (<= beta alpha)
		
		;; Insert a new node in the hash table with the newly computed
		;; inexact alpha value
		(when (or (not nodey)
			  (< curr-depth (ab-node-depth nodey)))
		  (setf (gethash key hashy)
		    (make-ab-node :depth curr-depth
				  :val alpha
				  :exact nil)))
		;; Return, pruning the other children
		(return-from compute-max new-alpha)))
	    )
	  )
	
	;; Insert a new node in the hash table with the newly computed alpha val
	(when (or (not nodey)
		  (< curr-depth (ab-node-depth nodey)))
	(setf (gethash key hashy)
	  (make-ab-node :depth curr-depth
			:val alpha
			:exact t)))
	
	;; At depth zero, return the best move and the root alpha value.
	;; At other depths, return alpha.
	(if (= curr-depth 0) (list best-move alpha) alpha))
      )
     )
    ))


  ;;  COMPUTE-MIN
  ;; -------------------------------------------------------
  ;;  INPUTS:  G, a CHESS struct
  ;;           CURR-DEPTH, the depth of this MIN node
  ;;           ALPHA, BETA, values received from parent MAX node
  ;;           STATTY, a stats struct
  ;;           CUTOFF-DEPTH, to limit depth of minimax search
  ;;           EVAL-FUNC, the evaluation function for G
  ;;           HASHY, a hash table representing the search tree
  ;;  OUTPUT:  The value of this MIN node according to rules
  ;;           of MINIMAX with ALPHA-BETA pruning

  (defun compute-min (g curr-depth alpha beta statty cutoff-depth eval-func hashy)
    (let* ((val nil)
	   (key (make-hash-key g))
	   (nodey (gethash key hashy)))
    (cond

     ;; If the game is over, indicate that we've won
     ((game-over? g) 
      (- *win-value* curr-depth))

     ;; If we've reached the cutoff-depth, we use the static evaluation
     ;; function to determine the current node's numerical value
     ((>= curr-depth cutoff-depth) 
      (funcall eval-func g))
     
     ;; If the current state already has a node in the tree, the node's depth
     ;; is less than the current depth, and either the node's beta value is
     ;; exact or less than the parent alpha
     ((and nodey
	   (<= (ab-node-depth nodey) curr-depth)
	   (or (ab-node-exact nodey)
	       (<= (ab-node-val nodey) alpha)))
      ;; Just return the value of the node already in the tree
      ;(format t "Min found an identical node. Node value: ~A, alpha: ~A~%"(ab-node-val nodey) alpha) 
      (ab-node-val nodey))

     ;; Otherwise,
     (t 
      ;; we use LEGAL-MOVES to fetch a list of legal moves. 
      (let ((moves (legal-moves g)))

	;; Increment the NUM-POTENTIAL-MOVES field in the STATS data
	;; structure by the number of legal moves.
	(incf (stats-num-potential-moves statty) (length moves))

	;; For each move in that list, 
	(dolist (mv moves)

	  ;; increment the number of moves done
	  (incf (stats-num-moves-done statty) 1)

	  ;; do the move
	  (apply #'do-move! g nil mv)

	  ;; make a recursive call to COMPUTE-MAX
	  (let ((new-beta 
		 (compute-max g 
			      (+ 1 curr-depth) alpha beta statty cutoff-depth eval-func hashy)))

	    ;; undo the move
	    (undo-move! g)

	    ;; If the value returned by COMPUTE-MAX is smaller than the previous
	    ;; beta, we update beta.
	    (when (< new-beta beta)
	      (setq beta new-beta)

	      ;; and see if pruning is possible.
	      (when (<= beta alpha) 
		;; Add a new node to the tree with the newly computed inexact
		;; beta value
		(when (or (not nodey)
			  (< curr-depth (ab-node-depth nodey)))
		  (setf (gethash key hashy) 
		    (make-ab-node :depth curr-depth
				  :val beta
				  :exact nil)))
		(return-from compute-min new-beta)))
	    )
	  )
	
	;; Add a new node to the tree with the newly computed exact beta value
	(when (or (not nodey)
		  (< curr-depth (ab-node-depth nodey)))
	  (setf (gethash key hashy)
	    (make-ab-node :depth curr-depth
			  :val beta
			  :exact t)))
	
	;; return beta
	beta)
      )
     )
    ))

