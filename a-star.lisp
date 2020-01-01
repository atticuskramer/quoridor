;;; =====================================================
;;;   CMPU-365, Spring 2017
;;;   Asmt. 3
;;;   Atticus Kramer
;;;   ATKRAMER-A-STAR.LISP
;;; =====================================================
;;;  Implementation of A-STAR algorithm.  Uses GEN-SEARCH
;;;  for the basic search engine.  Supplies a queuing function
;;;  based on a specified heuristic.

;;;  MAKE-F-QUEUING-FUNC
;;; ---------------------------------------------
;;;  INPUT:  HEURISTIC, a heuristic function (that applies to nodes)
;;;  OUTPUT:  A queuing function that inserts a list of new nodes
;;;           into a list of old nodes using a node-evaluation
;;;           function, F, derived from the supplied heuristic.

(defun make-f-queuing-func (heuristic)
  ;; F is the standard node evaluation function for A-STAR search
  ;; In particular, F(NODE) = G(NODE) + H(NODE)
  (labels ((f (node)
	     (+ (node-depth node) (funcall heuristic node))))
  
  (lambda (old-nodes new-nodes)
    (merge 'list 
	   old-nodes 
	   (sort new-nodes #'<= :key #'f) 
	   #'<=
	   :key #'f))) 
  )


;;;  A-STAR-SEARCH
;;; -------------------------------------------------
;;;  INPUTS:  PROB, a search problem
;;;           HEURISTIC, a heuristic function that is suitable
;;;              for the given kind of search problem (must be admissible)
;;;  OUTPUT:  The result of the search:  either a goal node or NIL.

(defun a-star-search (prob heuristic)
  (gen-search prob (make-f-queuing-func heuristic))
  )
