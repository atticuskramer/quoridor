;; ---------------------------------------
;;  Representation of quoridor game
;;  Atticus Kramer
;;  CMPU 365, Spring 2017
;;  quoridor.lisp
;; ---------------------------------------


;;  Functions Defined
;; ------------------------
;;  NEW-QUORIDOR
;;  COPY-ARRAY
;;  COPY-GAME
;;  OTHER-PLAYER
;;  PLAYER-POS
;;  PRINT-QUORIDOR
;;  PLAYER-HAS-WALLS?
;;  GAME-OVER?
;;  LEGAL-MOVE?
;;  MOVE-PIECE!

;;  QUOR-MAKE-HASH-KEY
;;  MOVE-IN-DIR
;;  MOVE-EAST
;;  MOVE-NORTH
;;  MOVE-SOUTH
;;  MOVE-WEST
;;  MAKE-QUORIDOR-PROB
;;  AS-THE-CROW-FLIES
;;  CROW-FLIES-HEURISTIC
;;  A-STAR-QUORIDOR

;;  SPACE-FOR-WALL?
;;  PATH-EXISTS?
;;  WALL-IN-DIR?
;;  LEGAL-WALL?
;;  PLACE-WALL!
;;  DO-MOVE!
;;  UNDO-MOVE!
;;  LEGAL-MOVES
;;  MAKE-HASH-KEY
;;  EVAL-FUNC-1
;;  EVAL-FUNC-2


;; GLOBAL CONSTANTS
;; ------------------------

;; the players

(defconstant *black* 0)
(defconstant *white* 1)

;; The possible orientations of a wall

(defconstant *empty* 0)
(defconstant *horizontal* 1)
(defconstant *vertical* 2)

;; The possible directions in which a player can move

(defconstant *north* 0)
(defconstant *east* 1)
(defconstant *south* 2)
(defconstant *west* 3)

;; The size of the board and the number of walls
;;
;; Note: the number of walls is determined by its ratio
;;       with the area of the board.

(defconstant *board-size* 5)
(defconstant *max-row* (1- *board-size*))
(defconstant *num-walls* 
    (round (/ (* 10 *board-size* *board-size*) 81)))


;; MOVE-TYPES FOR DO-MOVE!

(defconstant *move-piece* 0)
(defconstant *place-wall* 1)

;; Values for classifying how good a move is in LEGAL-MOVES and WALL-VALUE
(defconstant *best* 0)
(defconstant *good* 1)
(defconstant *ok* 2)
(defconstant *bad* 3)
(defconstant *terrible* 4)

;; WIN-LOSS VALUES
;;
;; Note: we will probably change these values;
;; I just put them here now so alpha-beta can compile.

(defconstant *win-value* 400000)
(defconstant *loss-value* -400000)

;; NEGATIVE and POSITIVE INFINITY

(defconstant *neg-inf* -10000000)
(defconstant *pos-inf*  10000000)


;;  QUORIDOR STRUCT
;; -------------------------
;;  WALLS, an 8x8 array storing information about the placed walls
;;  BLK-POS, the position of the black piece on the board
;;  WHT-POS, the position of the white piece on the board
;;  BLK-WALLS-LEFT, the number of walls black still has to place
;;  WHT-WALLS-LEFT, the number of walls white still has to place
;;  WHOSE-TURN, either *black* or *white* representing the current player

(defstruct (quoridor (:print-function print-quoridor))
  (walls (make-array (list *max-row* *max-row*) 
		     :initial-element *empty*))
  (blk-pos (list 0 (floor *board-size* 2)))
  (wht-pos (list *max-row* (floor *board-size* 2)))
  (blk-walls-left *num-walls*)
  (wht-walls-left *num-walls*)
  (whose-turn *black*)
  (move-history nil))


;;  NEW-QUORIDOR
;; -------------------------------------------
;;  INPUTS: None
;;  OUTPUTS: A newly initialized quoridor game struct

(defun new-quoridor
    ()
  (make-quoridor))


;;  COPY-ARRAY
;; -------------------------------------------------------------
;;  INPUTS:  ARRIE, a two-dimensional array
;;  OUTPUT:  A copy of ARRIE

(defun copy-array (arrie)
  (let (;; NEW-ARRAY:  this will be the copy
	(new-array (make-array (array-dimensions arrie))))
    ;; Walk through the rows and columns of the arrays
    (dotimes (row (array-dimension arrie 0))
      (dotimes (col (array-dimension arrie 1))
	;; Copy corresponding elements...
	(setf (aref new-array row col)
	  (aref arrie row col))))
    ;; Return the NEW-ARRAY
    new-array))


;;  COPY-GAME
;; -------------------------------------------------------------
;;  INPUTS:  GAME, a quoridor struct
;;  OUTPUT:  A copy of GAME

(defun copy-game (game)
  (make-quoridor 
   :walls (copy-array (quoridor-walls game))
   :blk-pos (copy-list (quoridor-blk-pos game))
   :wht-pos (copy-list (quoridor-wht-pos game))
   :blk-walls-left (quoridor-blk-walls-left game)
   :wht-walls-left (quoridor-wht-walls-left game)
   :whose-turn (quoridor-whose-turn game)
   :move-history (quoridor-move-history game)
   ))


;;  OTHER-PLAYER
;; ------------------------------------------------
;;  INPUTS: PLAYER, either *black* or *white*
;;  OUTPUT: The other value

(defun other-player
    (player)
  (if (= player *black*)
      *white*
    *black*))


;;  PLAYER-POS
;; ------------------------------------------
;;  INPUTS: GAME, a quoridor struct
;;          PLAYER, either *black* or *white*
;;  OUTPUT: The position on the board of the given player
;;          (fomatted as a list (row col)

(defun player-pos 
    (game player)
  (if (= player *black*)
      (quoridor-blk-pos game)
    (quoridor-wht-pos game)))


;;  PRINT-QUORIDOR
;; ----------------------------------------
;;  INPUTS: GAME, a quoridor struct
;;  OUTPUTS: None
;;  SIDE-EFFECTS: Prints the game board for the quoridor struct

(defun print-quoridor
    (game str d)
  (declare (ignore d))
  (let ((walls (quoridor-walls game))
	(blk-pos (quoridor-blk-pos game))
	(wht-pos (quoridor-wht-pos game)))
    ;; Print out the top edge
    (dotimes (i *board-size*)
      (format str "+---"))
    (format str "+~%")
    ;; Only looping through the rows and the edges in between, top and bottom
    ;; edge are printed separately
    (dotimes (i (1- (* *board-size* 2)))
      (cond
       ((oddp i)
	;; Do the things for the odd lines (edges)
	(format str "+")
	;; For each space
	(dotimes (j *board-size*)
	  (if
	      (or (unless (= j *max-row*)
		    (= *horizontal* (aref walls (floor i 2) j)))
		  (unless (= j 0)
		    (= *horizontal* (aref walls (floor i 2) (1- j)))))
	      (format str "===+")
	    (format str "---+")))
	(format str "~%")
	)
       (t
	(format str "|")
	;; Do the things for the even lines (board rows)
	(dotimes (j *board-size*)
	  (if (or (unless (or (= i (* 2 *max-row*)) 
			      (= j *max-row*))
		    (= *vertical* (aref walls (/ i 2) j)))
		  (unless (or (= i 0) (= j *max-row*))
		    (= *vertical* (aref walls (1- (/ i 2)) j))))
	      (format str " ~A $" (cond
				   ((and (= (first blk-pos) (/ i 2))
					 (= (second blk-pos) j))
				    "B")
				   ((and (= (first wht-pos) (/ i 2))
					 (= (second wht-pos) j))
				    "W")
				   (t
				    " ")))
	    (format str " ~A |" (cond
				 ((and (= (first blk-pos) (/ i 2))
				       (= (second blk-pos) j))
				  "B")
				 ((and (= (first wht-pos) (/ i 2))
				       (= (second wht-pos) j))
				  "W")
				 (t
				  " ")))))
	(format str "~%")
	)))
    ;; Print out the bottom edge
    (dotimes (i *board-size*)
      (format str "+---"))
    (format str "+~%")
    ;; Print out the other game information
    (format str "Black walls left: ~A, White walls left: ~A~%"
	    (quoridor-blk-walls-left game) (quoridor-wht-walls-left game))
    (format str "~A's turn!~%~%" (if (= (quoridor-whose-turn game) *black*)
				     "BLACK"
				   "WHITE"))))


;;  PLAYER-HAS-WALLS?
;; -------------------------------------------
;;  INPUTS: GAME, a quoridor struct.
;;          PLAYER, the player we're wondering about.
;;  OUTPUTS: True if the player we're wondering about
;;           has any walls left to place, NIL otherwise.

(defun player-has-walls? (game player)
  (if 
      (= *black* player)
      (> (quoridor-blk-walls-left game) 0)
    (> (quoridor-wht-walls-left game) 0)))


;;  GAME-OVER?
;; -----------------------------------------
;;  INPUTS: GAME, a quoridor struct
;;  OUTPUT: false if the game is not yet over, or one of *black*, *white* if
;;            it is, indicating the winner
;;  NOTES: We don't check for a state in which both black and white are in
;;           winning positions, since this should be impossible in an actual
;;           game

(defun game-over?
    (game)
  (cond
   ;; If black has reached row 8
   ((= (first (quoridor-blk-pos game)) *max-row*)
    ;; Black has won!
    *black*)
   ;; If white has reached row 0 
   ((= (first (quoridor-wht-pos game)) 0)
    ;; White has won!
    *white*)
   ;; Otherwise
   (t
    ;; No one has won
    nil)))


;;  LEGAL-MOVE?
;; -----------------------------------------
;;  INPUTS: GAME, a quoridor struct
;;          DIR, one of *north*, *south*, *east*, or *west*
;;  OUTPUT: T if the specified move for the current player is legal, false
;;            otherwise

(defun legal-move?
    (game dir)

  (let* 
      ((plr (quoridor-whose-turn game))
       (pos (player-pos game plr))
       (row (first pos))
       (col (second pos)))
    
    (and (not (wall-in-dir? game row col dir))
	 
	 (cond
	  ((= dir *north*)
	   (> row 0))
	  ((= dir *south*)
	   (< row *max-row*))
	  ((= dir *west*)
	   (> col 0))
	  ((= dir *east*)
	   (< col *max-row*))))))


;;  WALL-IN-DIR?
;; ----------------------------------------------------------
;;  INPUTS: GAME, a quoridor struct
;;          ROW, COL, ints in the range [0..boardsize-1]
;;          DIR, the direction in which we are checking for a wall

(defun wall-in-dir?
    (game row col dir)
  (let ((walls (quoridor-walls game)))
    (cond
     ((= dir *north*)
      (or (unless (or (= row 0) (= col 0))
	    (= (aref walls (1- row) (1- col)) *horizontal*))
	  (unless (or (= row 0) (= col *max-row*))
	    (= (aref walls (1- row) col) *horizontal*))))
     ((= dir *south*)
      (or (unless (or (= row *max-row*) (= col 0))
	    (= (aref walls row (1- col)) *horizontal*))
	  (unless (or (= row *max-row*) (= col *max-row*))
	    (= (aref walls row col) *horizontal*))))
     ((= dir *east*)
      (or (unless (or (= row 0) (= col *max-row*))
	    (= (aref walls (1- row) col) *vertical*))
	  (unless (or (= row *max-row*) (= col *max-row*))
	    (= (aref walls row col) *vertical*))))
     ((= dir *west*)
      (or (unless (or (= row 0) (= col 0))
	    (= (aref walls (1- row) (1- col)) *vertical*))
	  (unless (or (= row *max-row*) (= col 0))
	    (= (aref walls row (1- col)) *vertical*)))))))


;;  MOVE-PIECE!
;; -----------------------------------------
;;  INPUTS: GAME, a quoridor struct
;;          CHECK-LEGAL, boolean to indicate if the function should check if
;;            the move is legal to do
;;          DIR, one of *north*, *south*, *east*, or *west*
;;  OUTPUT: GAME, after doing the specified move for the current player, or
;;            NIL if CHECK-LEGAL is true and the move is illegal
;;  SIDE-EFFECTS: Destructively modifies GAME, doing the specified move
;;
;;  NOTE TO ATTICUS: I had to change the output to NIL when the move is
;;                   illegal because a-star search expects this feedback.

(defun move-piece! 
    (game check-legal dir)
  (let* ((plr (quoridor-whose-turn game))
	 (pos (player-pos game plr)))
    (cond
     ;; If check-legal is true and the move is illegal, return NIL
     ((and check-legal (not (legal-move? game dir)))
      (format t "Illegal move!") nil)
     ;; Otherwise, move the piece 1 space in the specified direction
     (t 
      (cond
       ((= dir *north*)
	(setf pos (list (1- (first pos)) (second pos))))
       ((= dir *east*)
	(setf pos (list (first pos) (1+ (second pos)))))
       ((= dir *south*)
	(setf pos (list (1+ (first pos)) (second pos))))
       (t
	(setf pos (list (first pos) (1- (second pos))))))

      (if (= (quoridor-whose-turn game) *black*)
	  (setf (quoridor-blk-pos game) pos)
	(setf (quoridor-wht-pos game) pos))

      ;; Set it to the other player's turn
      (setf (quoridor-whose-turn game) (other-player (quoridor-whose-turn game)))
      ;; Add move to move history
      (push (list *move-piece* dir) (quoridor-move-history game)))))
  ;; Return the game
  game)



;;//////////////////////////////////////////////////////////////////////////////
;;                     FUNCTIONS USED BY A-STAR SEARCH 
;;//////////////////////////////////////////////////////////////////////////////

;;  QUOR-STATES-EQ?
;; -------------------------------------------------------------
;;  INPUTS:  GAME1, GAME2, two quoridor structs
;;  OUTPUT:  T if the two games are in the same state, NIL otherwise
;;
;;  NOTE:  During an a-star search for the shortest path to the other
;;         side of the board, no one's turn will change and no walls 
;;         will be placed, so there's no reason to compare anything
;;         other than the locations of the players on the board. 

					; (defun quor-states-eq? (game1 game2)
					;   (and 
					;     (equal (quoridor-blk-pos game1) (quoridor-blk-pos game2))
					;     (equal (quoridor-wht-pos game1) (quoridor-wht-pos game2))))


;;  QUOR-MAKE-HASH-KEY
;; -------------------------------------------------------------
;;  INPUT:  GAME, a quoridor struct
;;  OUTPUT: A list of lists containing the positions of both players.
;;
;;  NOTE:  During an a-star search for the shortest path to the other
;;         side of the board, no one's turn will change and no walls 
;;         will be placed, so there's no reason to compare anything
;;         other than the locations of the players on the board. 

(defun quor-make-hash-key (game)
  (list (quoridor-wht-pos game) (quoridor-blk-pos game)))


;;  MOVE-IN-DIR
;; -------------------------------------------------------------
;;  INPUT:  GAME, a quoridor struct.
;;  OUTPUT:  GAME-COPY, the resulting game if the move was legal;
;;           otherwise NIL.
;;  NOTE: This function is distinct from move-piece! because it 
;;        does not destructively modify the input game and does
;;        not toggle whose turn it is.

(defun move-in-dir 
    (game dir)
  (let* (
	 (game-copy (copy-game game))
	 (plr (quoridor-whose-turn game-copy))
	 (pos (player-pos game-copy plr))
	 )
    (cond
     ;; If the move is illegal, return NIL
     ((not (legal-move? game-copy dir)) nil)
     ;; Otherwise, move the piece 1 space in the specified direction
     ((= dir *north*)
      (setf pos (list (1- (first pos)) (second pos))))
     ((= dir *east*)
      (setf pos (list (first pos) (1+ (second pos)))))
     ((= dir *south*)
      (setf pos (list (1+ (first pos)) (second pos))))
     (t
      (setf pos (list (first pos) (1- (second pos))))))
    (if (= (quoridor-whose-turn game-copy) *black*)
	(setf (quoridor-blk-pos game-copy) pos)
      (setf (quoridor-wht-pos game-copy) pos))
    game-copy))


;; MOVE-NORTH, MOVE-EAST, MOVE-WEST, MOVE-SOUTH
;; -------------------------------------------------------------
;;  INPUT:  GAME, a quoridor struct.
;;  OUTPUT:  The resulting game if the move was legal; otherwise NIL.

(defun move-north (game)
  (move-in-dir game *north*))

(defun move-east (game)
  (move-in-dir game *east*))

(defun move-south (game)
  (move-in-dir game *south*))

(defun move-west (game)
  (move-in-dir game *west*))


;;  MAKE-QUORIDOR-PROBLEM
;; ---------------------------------------------------
;;  INPUTS:  STATE, a quoridor struct.
;;  OUTPUT:  A search-problem struct for the given quoridor game.

(defun make-quoridor-prob (state)
  (make-search-problem
   :init-state state 
   :actions (list #'move-north #'move-east #'move-south #'move-west)
   :goal-test-func #'game-over? 
   :make-hash-key-func #'quor-make-hash-key)) 


;;  AS-THE-CROW-FLIES
;; --------------------------------------------------
;;  INPUT:  GAME, a quoridor struct.
;;  OUTPUT:  The distance from the current position of the player
;;           whose turn it is to their goal side of the board.

(defun as-the-crow-flies (game)
  (if 
      (= (quoridor-whose-turn game) *black*)
      (- *max-row* (first (quoridor-blk-pos game)))
    (first (quoridor-wht-pos game))))


;;  CROW-FLIES-HEURISTIC
;; --------------------------------------------------
;;  INPUT:  NODE, a node struct.
;;  OUTPUT:  The value of the as-the-crow-flies function
;;           as applied to NODE's STATE.

(defun crow-flies-heuristic (node)
  (as-the-crow-flies (node-state node)))


;;  A-STAR-QUORIDOR
;; --------------------------------------------------
;;  INPUTS:  STATE, a quoridor struct.
;;  OUTPUT:  The RESULTS struct of running a-star search to find 
;;           shortest path distance from the current position of
;;           the player whose turn it is to their goal side of the
;;           board.
;;  NOTE:  Uses an as-the-crow-flies heuristic.

(defun a-star-quoridor (state)
  (a-star-search (make-quoridor-prob state) #'crow-flies-heuristic))

;;//////////////////////////////////////////////////////////////////////////////



;;  SPACE-FOR-WALL?
;; ---------------------------------------------
;;  INPUTS: GAME, a quoridor struct
;;          DIR, either *horizontal* or *vertical* indicating how the piece
;;            should be placed
;;          ROW, COL, ints in the range [0,7] indicating the cross to place
;;            the wall at
;;  OUTPUT: T if it there is room to place a wall at the given location and 
;;          direction, false otherwise
;;
;;  NOTES:  Used as a helper function to LEGAL-WALL?

(defun space-for-wall?
    (walls dir row col)

  (if
      ;; If the wall we're about to place is vertical...
      (= *vertical* dir)

      ;; ...we make sure that there are no walls at the intersection 
      ;; where we're about to place the wall and no vertical
      ;; walls to the north or south of it. 
      (if 
	  (or 
	   (not (= *empty* 
		   (aref walls row col)))
	   (= *vertical*                  
	      (aref walls (min (1+ row) (1- *max-row*)) col))
	   (= *vertical* 
	      (aref walls (max 0 (1- row)) col)))

	  ;; If there's a wall conflict, we return nil.
	  nil
	;; Otherwise, there's no conflict so we return true. 
	t)

    ;; If the wall we're about to place is horizontal...
    (if 
	;; ...we make sure that there are no walls at the intersection 
	;; where we're about to place the wall and no horizontal
	;; walls to the east or west of it. 
	(or 
	 (not (= *empty* 
		 (aref walls row col)))  
	 (= *horizontal* 
	    (aref walls row (max 0 (1- col))))
	 (= *horizontal* 
	    (aref walls row (min (1+ col) (1- *max-row*)))))

	;; If there's a wall conflict, we return nil.
	nil
      ;; Otherwise, there's no conflict so we return true. 
      t)))


;;  PATH-EXISTS?
;; -------------------------------------------------
;;  INPUTS: GAME, a quoridor struct
;;          PLAYER, either *black* or *white*
;;  OUTPUT: True if a path exists from PLAYER's location to the other 
;;          player's starting side. False, otherwise

(defun path-exists? (game player)

  ;; We make a copy of the game so as not to erroneously modify the original.
  (let ((game-copy (copy-game game)))

    ;; We set it to be the desired player's turn
    (setf (quoridor-whose-turn game-copy) player)

    ;; If an a-star search doesn't return a goal node, we return NIL to 
    ;; indicate that no path exists.
    (if (null (results-goal-node (a-star-quoridor game-copy))) nil

      ;; Otherwise, a path must exist, so we return true.
      t)))


;;  SHORTEST PATH
;; ------------------------------------------------------
;;  INPUT:  GAME, a quoridor game struct.
;;          PLAYER, the player about whom we're inquiring.
;;  OUTPUT:  A number representing the length of the player's
;;           shortest path to the goal side. 

(defun shortest-path (game player)
  ;; We make a copy of the game so as not to erroneously modify the original.
  (let ((game-copy (copy-game game)))

    ;; We set it to be the desired player's turn
    (setf (quoridor-whose-turn game-copy) player)

    (let ((goal-node (results-goal-node (a-star-quoridor game-copy))))
      (when (node-p goal-node)
	(1- (length (build-path goal-node)))))))


;;  LEGAL-WALL?
;; ---------------------------------------------
;;  INPUTS: GAME, a quoridor struct
;;          DIR, either *horizontal* or *vertical* indicating how the piece
;;            should be placed
;;          ROW, COL, ints in the range [0,7] indicating the cross to place
;;            the wall at
;;  OUTPUT: T if it is legal to place a wall at the given location and 
;;          direction, false otherwise

(defun legal-wall?
    (game dir row col)

  ;; Result will hold whether the wall is legal. Set to false by default.
  (let ((legal nil))

    ;; We call our SPACE-FOR-WALL? helper function to make sure another
    ;; wall isn't in the way of where we want to put this one.
    (when (space-for-wall? (quoridor-walls game) dir row col)

      ;; If there's room, we temporarily place the wall there and then
      ;; check if there's still a path 
      (setf (aref (quoridor-walls game) row col) dir)

      ;; We only set legal to true if a path still exists for both players
      ;; to the other player's starting side. 
      (when 
          (and (path-exists? game *black*) (path-exists? game *white*))
	(setf legal t))

      ;; We make sure to undo that placement of the wall. 
      (setf (aref (quoridor-walls game) row col) *empty*))

    ;; We return whether or not the wall placement of legal. 
    legal))


;;  PLACE-WALL!
;; -------------------------------------------
;;  INPUTS: GAME, a quoridor struct
;;          CHECK-LEGAL, boolean to indicate if the function should check if
;;            the wall is legal to place
;;          DIR, either *horizontal* or *vertical* indicating how the piece
;;            should be placed
;;          ROW, COL, ints in the range [0,7] indicating the cross to place
;;            the wall at
;;  OUTPUT: GAME, modified to have placed one of the current player's walls
;;            at ROW,COL in direction DIR. If CHECK-LEGAL is true and the move
;;            is illegal, just returns the current game
;;  SIDE-EFFECT: Destructively modifies GAME in the indicated fashion

(defun place-wall! 
    (game check-legal dir row col)
  (cond
   ;; If we are checking for the legality of the wall and it is illegal
   ((and check-legal (not (legal-wall? game dir row col)))
    (format t "Illegal wall placement!")
    ;; just return the game
    game)
   ;; Otherwise, place the wall
   (t
    (setf (aref (quoridor-walls game) row col) dir)
    (if (= (quoridor-whose-turn game) *black*)
	(decf (quoridor-blk-walls-left game))
      (decf (quoridor-wht-walls-left game)))
    (setf (quoridor-whose-turn game) (other-player (quoridor-whose-turn game)))
    ;; Add move to move history.
    (push (list *place-wall* row col) (quoridor-move-history game))
    ;; Return the game.
    game)))


;;  DO-MOVE!
;; -------------------------------------------
;;  INPUTS:  GAME, quoridor struct
;;           CHECK-LEGAL?, a boolean flag
;;           MOVE-TYPE, either *move-piece* or *place-wall*
;;           DIR - the direction in which to move the piece if we're
;;                 moving a piece, or the direction the wall should 
;;                 be facing if we're placing a wall. 
;;           ROW, COL - the coordinates of the wall, if we're
;;           placing a wall. These arguments are optional.
;;  OUTPUT:  Resulting quoridor struct if move legal and made
;;           NIL otherwise
;;  NOTE:  Only checks legality of proposed move if CHECK-LEGAL? set

(defun do-move! (game check-legal? move-type dir &optional row col)
  (cond 
   ((= move-type *move-piece*)
    (move-piece! game check-legal? dir))
   ((= move-type *place-wall*)
    (place-wall! game check-legal? dir row col))
   (t (format t "invalid move-type")
      nil)))


;;  UNDO-MOVE!
;; -------------------------------------------
;;  INPUTS:  GAME, quoridor struct
;;  OUTPUT:  Resulting quoridor struct after undoing the last move.

(defun undo-move! (game)
  (cond
   ;; Case 1: No moves on move history.
   ((null (quoridor-move-history game))
    (format t "Can't undo move; empty history!~%"))
   ;; Case 2: There's a move to be undone.
   (t 
    ;; We grab the move, its type, and the player who moved. 
    (let* (
	   (move (pop (quoridor-move-history game)))
	   (move-type (first move))
	   (plr (other-player (quoridor-whose-turn game)))
	   )
      (cond 
       ;; Case 2A: We're undoing a piece move.
       ((= move-type *move-piece*)

	;; We grab the direction the player moved
	;; and the player's current position.
	(let (
	      (dir (second move))
	      (pos (player-pos game plr))
	      )
	  ;; We move the player in the opposite direction of the
	  ;; direction they moved in order to put them back where
	  ;; they were. 
	  (cond
	   ((= dir *south*)
	    (setf pos (list (1- (first pos)) (second pos))))
	   ((= dir *west*)
	    (setf pos (list (first pos) (1+ (second pos)))))
	   ((= dir *north*)
	    (setf pos (list (1+ (first pos)) (second pos))))
	   (t
	    (setf pos (list (first pos) (1- (second pos))))))
	  (if (= plr *black*)
	      (setf (quoridor-blk-pos game) pos)
	    (setf (quoridor-wht-pos game) pos))))
       ;; Case 2B: We're un-placing a wall. 
       (t 
	(setf (aref (quoridor-walls game)
		    (second move) (third move)) *empty*)
	(if (= plr *black*)
	    (incf (quoridor-blk-walls-left game))
	  (incf (quoridor-wht-walls-left game)))))
      ;; We change it to this previous player's turn.
      (setf (quoridor-whose-turn game) plr))))
  ;; Return the modified quoridor struct.
  game)


;;  LEGAL-MOVES
;; ------------------------------------------------------
;;  INPUT:  GAME, a quoridor game struct
;;  OUTPUT:  A list of the legal moves for whoever's turn it is.

(defun legal-moves (game)

  ;; Let legal-moves-acc be our list of legal moves
  (let (
  	(legal-moves-acc '())
  	(dims (array-dimensions (quoridor-walls game)))
    (walls (quoridor-walls game))
    (player (quoridor-whose-turn game))
    (best-walls nil)
    (good-walls nil)
    (ok-walls nil)
    (bad-walls nil)
    (terrible-walls nil)
    )

  (when (player-has-walls? game player)

      ;; We iterate over every location in the walls array.
      ;; If we find a place where we could legally place a wall,
      ;; we add the corresponding *place-wall* move to the accumulator.
      (dotimes (row (first dims))
       (dotimes (col (second dims))

        (let* (
          (val (wall-value game nil row col))
          (near-player (adjacent-to-player game row col))
          )

        (when 
          (or near-player
            (adjacent-to-another walls *vertical* row col))
          (cond 
           ((= val *best*)
            (when (legal-wall? game *vertical* row col)
              (push (list *place-wall* *vertical* row col)
                best-walls)))
           ((= val *good*)
            (when (legal-wall? game *vertical* row col)
              (push (list *place-wall* *vertical* row col)
                good-walls)))
           ((= val *ok*)
            (when (legal-wall? game *vertical* row col)
              (push (list *place-wall* *vertical* row col)
                ok-walls)))
           (t
            (when (legal-wall? game *vertical* row col)
              (push (list *place-wall* *vertical* row col)
                bad-walls)))))

        (when 
          (or near-player 
            (adjacent-to-another walls *horizontal* row col))
          (cond 
           ((= val *best*)
            (when (legal-wall? game *horizontal* row col)
              (push (list *place-wall* *horizontal* row col)
                best-walls)))
           ((= val *good*)
            (when (legal-wall? game *horizontal* row col)
              (push (list *place-wall* *horizontal* row col)
                good-walls)))
           ((= val *ok*)
            (when (legal-wall? game *horizontal* row col)
              (push (list *place-wall* *horizontal* row col)
                ok-walls)))
           (t
            (when (legal-wall? game *horizontal* row col)
              (push (list *place-wall* *horizontal* row col)
                bad-walls)))))
        ))))

    ;; We also check if we could legally move in each direction and,
    ;; if so, add the corresponding *move-piece* move to the accumulator.
    (when (legal-move? game *north*)
      (push (list *move-piece* *north*) legal-moves-acc))
    (when (legal-move? game *west*)
      (push (list *move-piece* *west*) legal-moves-acc))
    (when (legal-move? game *south*)
      (push (list *move-piece* *south*) legal-moves-acc))
    (when (legal-move? game *east*)
      (push (list *move-piece* *east*) legal-moves-acc))

    ;; Return the list of legal moves.
    (append legal-moves-acc best-walls good-walls ok-walls bad-walls)))


;;  WALL-VALUE
;; --------------------------------------------------
;;  INPUTS: GAME, a quoridor struct
;;          DIR, either *horizontal* or *vertical*
;;          ROW, COL, position of the wall (ints in [0,maxrow])

(defun wall-value (game dir row col)
  (let* ((player (quoridor-whose-turn game))
	 (other (other-player player))
	 (opp-pos (player-pos game other))
	 (opp-row (- (first opp-pos) 0.5))
	 (opp-col (- (second opp-pos) 0.5))
	 (distance (sqrt
		    (+
		     (* (- opp-row row) (- opp-row row))
		     (* (- opp-col col) (- opp-col col))))))
    ;; Find the distance between the wall and the opponent
    ;; A shorter distance means (hopefully) that the wall is more
    ;; likely to be a good move to choose
    (cond
     ((< distance 1)
      *best*)
     ((< distance 2.5)
      *good*)
     ((< distance 5)
      *ok*)
     (t
      *bad*))
    ))


;;  ADJACENT-TO-ANOTHER
;; --------------------------------------------------
;;  INPUTS: WALLS, a quoridor struct
;;          DIR, either *horizontal* or *vertical*
;;          ROW, COL, position of the wall (ints in [0,maxrow])

(defun adjacent-to-another(walls dir row col)
	(let ((other-dir (if (= dir *horizontal*) *vertical* *horizontal*)))
		(or 
			(= other-dir
				(aref walls (min (1+ row) (1- *max-row*)) col)) ;; row++
			(= other-dir
				(aref walls (max 0 (1- row)) col)) ;; row--
			(= other-dir
				(aref walls row (max 0 (1- col)))) ;; coll--
			(= other-dir
				(aref walls row (min (1+ col) (1- *max-row*)))) ;; col ++
			(= other-dir
				(aref walls (min (1+ row) (1- *max-row*)) (min (1+ col) (1- *max-row*)))) ;; row++, col++
			(= other-dir
				(aref walls (max 0 (1- row)) (max 0 (1- col)))) ;; row--, col--
			(= other-dir
				(aref walls (min (1+ row) (1- *max-row*)) (max 0 (1- col)))) ;; row++, col--		
			(= other-dir
				(aref walls (max 0 (1- row)) (min (1+ col) (1- *max-row*)))) ;; row--, col++

			(if (= dir *vertical*)
				(or 
					(not (= *empty* 
						(aref walls (min (+ row 2) (1- *max-row*)) col)))
					(not (= *empty*
						(aref walls (max 0 (- row 2)) col))))
				(or 
					(not (= *empty*
						(aref walls row (max 0 (- col 2)))))
					(not (= *empty*
						(aref walls row (min (+ col 2) (1- *max-row*))))))))))


;;  ADJACENT-TO-PLAYER
;; ---------------------------------------------------
;;  INPUTS: GAME, a quoridor struct
;;          ROW, COL, the position of a wall to check
;;                    (ints in the range [0, maxrow))
;;  OUTPUT: True if the wall position is adjacent to either player,
;;          NIL otherwise

(defun adjacent-to-player (game row col)
  (let* ((player (quoridor-whose-turn game))
	 (pos (player-pos game player))
	 (plr-row (first pos))
	 (plr-col (second pos))
	 (opp-pos (player-pos game (other-player player)))
	 (opp-row (first opp-pos))
	 (opp-col (second opp-pos))
	 (max-wall-row (1- *max-row*)))
    (or
     ;; Check the bottom right of the player
     (and (= row (min plr-row max-wall-row))
	  (= col (min plr-col max-wall-row)))
     ;; Check the bottom left of the player
     (and (= row (min plr-row max-wall-row))
	  (= col (max (1- plr-col) 0)))
     ;; Check the top right of the player
     (and (= row (max (1- plr-row) 0))
	  (= col (min plr-col max-wall-row)))
     ;; Check the top left of the player
     (and (= row (max (1- plr-row) 0))
	  (= col (max (1- plr-col) 0)))
     ;; Same as above but for opponent
     (and (= row (min opp-row max-wall-row))
	  (= col (min opp-col max-wall-row)))
     (and (= row (min opp-row max-wall-row))
	  (= col (max (1- opp-col) 0)))
     (and (= row (max (1- opp-row) 0))
	  (= col (min opp-col max-wall-row)))
     (and (= row (max (1- opp-row) 0))
	  (= col (max (1- opp-col) 0))))))


;;  MAKE-HASH-KEY
;; -------------------------------------------------
;;  INPUTS: GAME, a quoridor struct
;;  OUTPUT: A hash key to be used by alpha-beta search

(defun make-hash-key (game)
  game)



;;  EVAL-FUNC-1
;; -------------------------------------------------
;;  INPUT:  GAME, a quoridor struct
;;  OUTPUT:  The static evaluation of the current state of the game
;;           based on the difference in the lengths of the shortest
;;           paths each of the players could take to their goal side.
;;           Adds a bonus point to the player whose turn it is.

(defun eval-func-1 (game)
  (let* (
	 (player (quoridor-whose-turn game))
	 (other (other-player player))
	 )
    ;; We return the opponent's distance minus the current player's distance.
    ;; We add one to the other player's distance since it's not that player's
    ;; turn. 
    (- (shortest-path game other) (1+ (shortest-path game player)))))


;;  EVAL-FUNC-2
;; -------------------------------------------------
;;  INPUT:  GAME, a quoridor struct
;;  OUTPUT:  The static evaluation of the current state of the game
;;           based on the difference in the lengths of the shortest
;;           paths each of the players could take to their goal side.

(defun eval-func-2 (game)
  (let* (
	 (player (quoridor-whose-turn game))
	 (other (other-player player))
	 )
    ;; We return the opponent's distance minus the current player's distance.
    (- (shortest-path game other) (shortest-path game player))))


;;  FOOLISHLY-AGRESSIVE
;; -------------------------------------------------
;;  INPUT:  GAME, a quoridor struct
;;  OUTPUT:  The static evaluation of the current state of the game
;;           based only on the length of the other player's shortest
;;           path to its goal.

(defun foolishly-aggressive (game)
  (let* (
	 (player (quoridor-whose-turn game))
	 (other (other-player player))
	 )
    (shortest-path game other)))


;;  FOOLISHLY-OPTIMISTIC
;; --------------------------------------------------
;;  INPUT: GAME, a quoridor struct
;;  OUTPUT:  The static evaluation of the current state of the game
;;           based only on the length of the current player's shortest
;;           path to its goal.

(defun foolishly-optimistic (game)
  (let ((player (quoridor-whose-turn game)))
    (- (* *board-size* *board-size*) (shortest-path game player))))