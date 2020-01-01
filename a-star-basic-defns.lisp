;; ========================================
;;   CMPU-365, Spring 2017
;;   Asmt. 3 
;; ========================================
;;   FILE:  basic-defns.lisp 
;; ========================================
;;   Data structures and related low-level functions in preparation
;;   for implementing iterative-deepening and a-star search algorithms.
;; ---------------------------------------------------------------------
;;   This file is similar to basic-defns.lisp from asmt 2, except that
;;   it also includes solutions for CYCLE?, EXPAND, and
;;   MAKE-ROOT-NODE, and defines a wrapper for depth-limited search,
;;   called DLS, instead of a wrapper for general-search.

;;  CONTENTS
;; --------------------------------------------------------------
;;  SEARCH-PROBLEM struct
;;  NODE struct, and functions on nodes:  PRINT-NODE, BUILD-PATH
;;  RESULTS struct and function:  PRINT-RESULTS
;;  FRONT-ENQUEUE! (queuing function for DFS/DLS/IDS)
;;  CYCLE?, MAKE-ROOT-NODE, EXPAND, GEN-SEARCH-GUTS (solns for asmt2)
;;  GEN-SEARCH (to be used for A-STAR, *NOT* for DLS or IDS)
;;  *CUTOFF* (global constant for DLS)
;;  DLS (wrapper function for DLS-GUTS)

;; --------------------------------------
;;  The PROBLEM struct 
;; --------------------------------------

(defstruct search-problem
  init-state		; the initial state
  actions		; the list of actions
  goal-test-func	; the goal test function
  make-hash-key-func ; makes a hash key from a state
  )

;; --------------------------------------
;;  The NODE struct 
;; --------------------------------------
;;  Note the use of the :PRINT-FUNCTION keyword argument that enables
;;  us to specify which function should be used to display a NODE
;;  struct.  (Kind of like providing a "toString" method in Java.)

(defstruct (node (:print-function print-node))
  state			; the state associated with this node
  (parent nil)		; the parent node for this node
  (action nil)		; the most recent action performed
  (depth 0)		; the depth of this node
  )

;;  PRINT-NODE  --  print function for NODEs 
;; ------------------------------------------------------------------
;;  Note:  The three inputs specified below are required if we
;;         want to use this function as the printing function for
;;         the NODE struct defined above.
;; ------------------------------------------------------------------
;;  INPUTS:  NODE, a search node
;;           STR, an output stream (usually t)
;;           DEPTH, printing depth parameter (ignored)
;;  OUTPUT:  None
;;  SIDE EFFECT:  Displays given NODE to the given output stream.

(defun print-node (node str depth)
  ;; To avoid compiler warnings about an input we never use.
  (declare (ignore depth))
  
  (let ((st (node-state node)))
    ;; Display the most recent action (i.e., the one that got us here)
    (format str "NODE:  (action = ~A)~%" (node-action node))
    ;; Display the current state
    (format str "         STATE: ~A~%" st)))

;;  BUILD-PATH
;; -----------------------------------------------------------
;;  INPUT:   GOAL-NODE
;;  OUTPUT:  A list of nodes obtained by tracing the parent
;;           links starting from the goal-node all the way
;;           back to the root node

(defun build-path (goal-node)
  
  ;; BUILD-PATH-ACC is a recursive helper function
  ;; Note the use of LABELS (similar to LETREC in Scheme)
  
  (labels ((build-path-acc (node acc)
	     ;; Base Case:  NODE is NIL 
	     ;;   This happens when previous function call involved
	     ;;   the root node.  The root node's parent is NIL.
	     ;;   In this case, we are done!
	     (if (not node)
		 ;; So return the accumulator
		 acc
	       ;; Recursive Case:  
	       ;;   Accumulate the current node as we move
	       ;;   to the PARENT...
	       (build-path-acc (node-parent node) (cons node acc)))))
    
    ;; Body of LABELS:  call the recursive helper with ACC=NIL
    (build-path-acc goal-node nil)))

;; ------------------------------------------------------------
;;  RESULTS struct -- used to keep track of info during search 
;; ------------------------------------------------------------
;;  The start time, end time, and elapsed time are all measured by
;;  "internal-run-time" which is typically measured in milliseconds.
;;  See the global constant INTERNAL-TIME-UNITS-PER-SECOND, which
;;  equals 1000 on our system.
;; -----------------------------------------------------------
;;  Note the PRINT-RESULTS print function

(defstruct (results (:print-function print-results))
  (goal-node nil)       ; a goal node, if found; otherwise, NIL or *CUTOFF*
  (num-nodes 0)         ; the number of nodes generated during the search
  (start-time NIL)      ; when the search started (msecs)
  (end-time NIL)        ; when the search stopped (msecs)
  (elapsed-time NIL)    ; how long the search took (msecs)
  )

(defun print-results-stats (rezzy str)
  ;; helper for print-results
  ;; only prints timing and num-nodes info, does not call BUILD-PATH
  (let ((nodey (results-goal-node rezzy))
	(num-nodes (results-num-nodes rezzy))
	(elapsed-time (/ (results-elapsed-time rezzy)
			 internal-time-units-per-second
			 1.0)))
    (if (node-p nodey)
	(format str "Search Result:  FOUND A GOAL NODE!~%")
      (format t "Search Result:  ~A~%" nodey))
    
    (format str "Generated ~A nodes " num-nodes)
    (if (> num-nodes 0)
	(format str "in ~A seconds (~A sec/node)~%"
		elapsed-time (/ elapsed-time num-nodes))
      (format str "~%"))))

;;  PRINT-RESULTS  --  print function for RESULTS structs
;; -------------------------------------------------------------
;;  INPUTS:  REZZY, a RESULTS struct
;;           STR, the stream to print to (usually T for us)
;;           DEPTH, ignored by us
;;  OUTPUT:  Nothing
;;  SIDE EFFECT:  If REZZY's GOAL-NODE is NIL, then it prints out a failure
;;    message.  Otherwise, it prints out a success message and displays the
;;    sequence of nodes from the ROOT all the way to the GOAL-NODE.

(defun print-results (rezzy str depth)
  ;; To avoid compiler warnings about an input we never use
  (declare (ignore depth))

  (print-results-stats rezzy str)
  
  (let ((nodey (results-goal-node rezzy)))
     (when (node-p nodey)
       ;; use BUILD-PATH to generate the list of nodes from the 
       ;; root node to NODEY, then display these nodes.
       (format str "*** BEGIN SOLUTION PATH ***~%~%")
       (let* ((soln-path (build-path nodey))
	      (num-steps (1- (length soln-path))))
	 (format str "~A~%" soln-path)
	 (format str "==============================================~%")
	 (format str "*** END SOLUTION PATH (length: ~A) ***~%" num-steps)
	 (format str "==============================================~%")))))
     
;;  FRONT-ENQUEUE!  --  used by DLS-GUTS
;; -------------------------------------------------
;;  INPUT:  OLD-NODES, a list of old nodes (i.e., old search queue)
;;          NEW-NODES, a list of new nodes (i.e., new child nodes)
;;  OUTPUT:  A list containing new-nodes (at the front) and 
;;           old nodes (at the rear)

(defun front-enqueue! (old-nodes new-nodes)
  ;; NCONC is a destructive version of APPEND (see Paul Graham's book)
  ;; We use it here for efficiency reasons
  (nconc new-nodes old-nodes))


;;   CYCLE?  --  from asmt2 solutions
;; -------------------------------------------------------------
;;  INPUTS:  STATE, a problem state
;;           NODE, a search node
;;           STATE-EQ-FUNC, a function that determines whether
;;             two states are equal
;;  OUTPUT:  T if the given STATE is the same as the state of
;;    the given NODE or the state of any of NODE's ancestors.
;;    Uses STATE-EQ-FUNC to determine whether two states are equal.
;;    Otherwise returns NIL. 
;;    NOTE:  If NODE is NIL it returns NIL.

; (defun cycle? (state node state-eq-func)
;   ;; Returns T if NODE is an instance of a NODE structure...
;   (and node
;        ;; ... and either STATE is equal to NODE's state
;        (or (funcall state-eq-func state (node-state node))
; 	   ;; ... or STATE is equal to a state of one of NODE's ancestors
; 	   (cycle? state (node-parent node) state-eq-func))))


;;  MAKE-ROOT-NODE  --  from asmt2 solutions
;; ----------------------------------------------------------
;;  INPUT:  PROB, a search problem
;;  OUTPUT:  A search NODE that will be the ROOT NODE of a
;;           brand new search tree.

(defun make-root-node (prob)
  ;; The default values are okay for every field except STATE,
  ;; which needs to be the initial state specified by PROB.
  (make-node :state (search-problem-init-state prob)))


;;  EXPAND  --  from asmt2 solutions
;; ---------------------------------------------------------
;;  INPUTS:  NODE, a search node
;;           ACTS, a list of actions
;;           ST-EQ-FUNC, a function for testing whether two
;;              states are equal
;;           REZZY, a RESULTS struct
;;  OUTPUT:  A list of child nodes obtained by applying the
;;           given list of actions to NODE's state.  However, it
;;           does *NOT* include child nodes whose states already
;;           appear on the path from the root node to NODE.
;;           (Uses CYCLE? to determine this.)

(defun expand (node acts make-hash-key-func rezzy hashy)

  ;; LABELS is analogous to LETREC in Scheme
  ;; Here, it is used to define an accumulator-based helper
  ;; function, called EXPAND-ACC, that is used to accumulate
  ;; the newly created child nodes.
  ;;   Notice that by using LABELS to define EXPAND-ACC
  ;; inside the body of the EXPAND function definition, the
  ;; NODE, ACTS and ST-EQ-FUNC are available for use by EXPAND-ACC.
  ;; If you define EXPAND-ACC as a stand-alone helper function,
  ;; then you would need to provide additional explicit inputs
  ;; to make NODE, ACTS and ST-EQ-FUNC available, as needed.
  
  (labels
      ;; ACCUMULATOR-BASED HELPER FUNCTION:  EXPAND-ACC
      ;;   INPUTS:  ACTIONS, the actions that have not yet been applied
      ;;            CHILDREN, the accumulator of child nodes
      ;;   OUTPUT:  The accumulated list of child nodes
      ;; ------------------------------------------------------
      ;;  Note that the syntax of a LABELS function does not
      ;;  involve the use of LAMBDA; so it is just like DEFUN in
      ;;  that respect...
  
      ((expand-acc (actions children)
	 
	 (cond
  
	  ;; BASE CASE:  No more ACTIONS to apply; so we're done... 
	  ((null actions)
	   ;; increment global counter by the number of accumulated child nodes
	   (incf (results-num-nodes rezzy) (length children))
	   ;; return the accumulated list of child nodes
	   children)
	  
	  ;; RECURSIVE CASE:  More actions left to apply
	  (t
	   (let* (;; NEW-STATE is the state that results from applying
		  ;; the first action to NODE's state
		  (new-state (funcall (first actions) (node-state node)))
		  ;; The hash key corresponding to the new state.
		  (key (funcall make-hash-key-func new-state))
		  ;; UPDATED-CHILDREN will be the updated accumulator
		  ;; for the next recursive function call.
		  (updated-children 
		   (cond
		    ;; Case 1:  NEW-STATE is non-NIL and it is not a repeated
		    ;;          state (as determined by CYCLE?)
		    ((and new-state
			  (null (gethash key hashy)))
		     ;; so create a new node struct and CONS it onto 
		     ;; the front of the accumulator
		     (setf (gethash key hashy) t)
		     (cons (make-node
			    :state new-state
			    :parent node
			    :depth (1+ (node-depth node))
			    :action (first actions))
			   children))
		    ;; Case 2:
		    (t
		     ;; otherwise, just keep CHILDREN the same
		     children))))
	     
	     ;; make recursive function call on rest of ACTIONS
	     ;; and updated accumulator of child nodes
	     (expand-acc (rest actions) 
			 updated-children))))))
    
    ;; Call recursive helper with accumulator set to NIL
    (expand-acc acts nil)))


;; ---------------------------------------------------------------
;;  GEN-SEARCH-GUTS  --  from asmt2 solutions
;; ---------------------------------------------------------------
;;  INPUTS: PROBLEM, a search problem
;;          QUEUE-FN, a queuing function (used to insert newly
;;            created nodes into the search queue).  The queuing
;;            function determines which kind of search we're doing.
;;          REZZY, a RESULTS struct
;;  OUTPUT: The RESULTS struct, with its fields updated to include
;;          info about the search (e.g., num nodes explored; the goal
;;          node, if found; and timing information).
;; ---------------------------------------------------------------
;;  This function performs the indicated search problem using
;;  the search strategy determined by QUEUE-FN.

(defun gen-search-guts (problem queue-fn rezzy)
  
  (let* ((gt-func (search-problem-goal-test-func problem))
	(make-hash-key-func (search-problem-make-hash-key-func problem))
	 (acts (search-problem-actions problem))
	 ;; This equality can maybe be better? Don't need to check
	 ;; walls, just player positions.  DEFAULT STRUCT EQUALITY?!?!
	(hashy (make-hash-table :test #'equal)))
    
    (labels 
	;;  GEN-REC  --  local tail-recursive helper function
	;; ----------------------------------------------------
	;;  INPUT:  QUEUE, a list of nodes (the search queue)
	;;  OUTPUT:  The (destructively modified) RESULTS struct, REZZY
	((gen-rec (queue) 

	   (cond

	    ;; Base Case 1:  The search-queue is empty
	    ((null queue)
	     ;; Return REZZY, with GOAL-NODE=NIL, indicating failure
	     rezzy)

	    ;; Base Case 2:  The first node in the queue is a GOAL node
	    ((funcall gt-func (node-state (first queue)))
	     ;; Set GOAL-NODE in the RESULTS struct to that goal node, success!
	     (setf (results-goal-node rezzy) (first queue))
	     ;; Return the results
	     rezzy)

	    ;; Recursive Case:  More work to do
	    (t
	     ;; Expand first node in the queue (i.e., generate new
	     ;; child nodes).  Then use the QUEUE-FN to insert those 
	     ;; new nodes into the REST of the queue.  The QUEUE-FN
	     ;; determines whether you're doing breadth-first or
	     ;; depth-first search.
	     (gen-rec (funcall queue-fn 
			       (rest queue)
			       (expand (first queue)
				       acts
				       make-hash-key-func
				       rezzy
				       hashy)))))))
      
      ;; BODY of LABELS (i.e., LETREC)
      ;; Call the recursive helper function with initial search queue
      (gen-rec (list (make-root-node problem))))))

;;  GEN-SEARCH -- wrapper function for GEN-SEARCH-GUTS
;; --------------------------------------------------------
;;   INPUTS:  PROBLEM, a search problem
;;            QUEUE-FN, a queuing function
;;   OUTPUT:  A RESULTS data structure, whose contents report
;;            the results of the search.
;; --------------------------------------------------------
;;   NOTE: This is just a wrapper for GEN-SEARCH-GUTS (which you will
;;   define in "gen-search-starter.lisp").  GEN-SEARCH-GUTS does most
;;   of the work.  The wrapper function creates a new RESULTS data
;;   structure, initialized with NUM-NODES = 0 and START-TIME fetched
;;   from the operating system.

(defun gen-search (problem queue-fn)

  (let (;; Initial RESULTS struct 
	(rezzy (make-results :start-time (get-internal-run-time))))

    ;; GEN-SEARCH-GUTS will update the contents of REZZY:
    ;;   GOAL-NODE will either be NIL (failure) or a goal NODE,
    (gen-search-guts problem queue-fn rezzy)

    ;; Update END-TIME and ELAPSED-TIME
    (setf (results-end-time rezzy) (get-internal-run-time))
    (setf (results-elapsed-time rezzy) (- (results-end-time rezzy)
					  (results-start-time rezzy)))
    
 ;    (cond
     
 ;     ;; Case 1:  The search returned a node... presumably a GOAL node!
 ;     ((node-p (results-goal-node rezzy))
 ;      (format t "Hey!  We found a goal node!!~%"))

 ;     ;; Case 2:  The search returned something else... indicating failure
 ;     (t
 ;      (format t "Uh oh... queue empty!~%")))
    
 ;    ;; Report statistics     
 ;    (let ((num-nodes (results-num-nodes rezzy))
	;   (elapsed-time (/ (results-elapsed-time rezzy)
	; 		   internal-time-units-per-second
	; 		   1.0)))
 ;      (format t "~%Generated ~A nodes " num-nodes)
 ;      (if (> num-nodes 0)
	;   (format t "in ~A seconds (~A sec/node)~%"
	; 	  elapsed-time (/ elapsed-time num-nodes))
	; (format t "~%")))
      
    ;; Return the search-result 
    ;;(format t "~A~%" rezzy)
    rezzy
    ))
