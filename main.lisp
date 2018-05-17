(defconstant *rules* 
  '(
   (0 1 4 5)
   (0 1 5)
   (2 3 6)
   (2 3 6 7)
   (0 4 5)
   (0 2 5 8)
   (1 3 6 9)
   (3 6 7)
   (8 9 12)
   (4 6 9 12)
   (5 7 10 13)
   (10 11 15)
   (8 9 12 13)
   (9 10 12 13)
   (9 10 14 15)
   (10 11 14 15)))

(defconstant *elms* 16)
(defconstant *elms_per_row* 4)

(defun Play ()
  (let (Game RndSt Move (StepCnt 0))
    (setf RndState (make-random-state t)
          Game (Init_Game RndState))
    (format t "~%~5TStart state ~%~%")
    (Display_Game Game)
    (loop
     (when (Check_Win Game) (return))
     (setf Move (Choose_Move)
           Game (Do_Move Game Move)
           StepCnt (1+ StepCnt))
     (format t "~%~5TStep ~2D   Pressed ~D ~%" StepCnt Move)
     (Display_Game Game))
    (Congratulations StepCnt)))
          

(defun Display_Game (G)
  (let (value) 
    (sort G #'<)
    (dotimes (number *elms* value) 
      (progn
        (if (member number G) (princ "X")
          (princ "O"))
        (princ "|")
        (when (eq (mod (1+ number) *elms_per_row*) 0) (format t "~%")))))) 

; I wanted to do it recursively just for practice. definitly wasnt needed
;'(0 1 4 5)) test Game that takes one move
;'(0 1 4 5 2 3 6)) test game that takes two moves
(defun Init_Game (&optional (RS (make-random-state t)) (Count 0))
 ;'(0 1))
 (if (eql Count *elms*) NIL
   (let ((randNum (random 2)))
     (if (eql randNum 1) 
       (cons Count (Init_Game RS (1+ Count)))
       (Init_Game RS (1+ Count))))))
  

(defun Choose_Move()
  (progn
    (format t "~%Please Enter a number (0-~A): " (1- *elms*))
    (let ((input (read)))
      (if (and (>= input 0) (<= input (1- *elms*))) input (Choose_Move)))))
  
(defun Check_Win(G)
  (if (null G) T NIL))

(defun Do_Move(G Move)
  (let ((rule (nth Move *rules*)))
    (Turn_To_X G rule (Turn_To_O G rule))))

(defun Turn_To_O(G rule &optional Res)
  (cond 
   ((null G) Res)
   ((member (car G) rule) (Turn_To_O (cdr G) rule Res))
   (T (Turn_To_O (cdr G) rule (cons (car G) Res)))))

(defun Turn_To_X(G_Old rule &optional Res)
  (cond
   ((null rule) Res)
   ((not (member (car rule) G_Old)) (Turn_To_X G_Old (cdr rule) (cons (car rule) Res)))
   (T (Turn_To_X G_Old (cdr rule) Res)))) 

(defun Congratulations (Steps)
  (format t "~%Congratulations! You beat the game in ~D steps" Steps))

; AI PLAYER PORTION

; Explanation of the algorithm:
; Essentially it just does a breadth first search with a few constraints.
; It requires that no loops are made. Meaning that no set of moves like (1 2 3 1 2 3) could be made
; or (1 2 1 2) or (1 2 2). All of these are invalid. It also requires that at least one x is flipped 
; per turn. This might not be the best idea for finding the absolute shortest path, but it does
; cut down on the number of options significantly.
; Also if the number of paths gets above the max path count, at attempts to prune any that have lengths (amount of X's)
; over the median. This doesn't mean that the path count will necessarily go below the maxPath amount
; it just means we'll get rid of the games with the most X's so all remaining boards are at least as good as the median
; 
; Improvements that could be made:
; I think it would've been wise to give paths some type of score as well as the moves applied to them.
; Such as if a move got rid of a lot of X's it would have a higher score and that path would be explored first
; Once it's score became lower than another path's, the other path would start to be evaluated. I think this would
; be more similar to a dikstras or A* algorithm rather than a naive breadth first. But mine works, it just isn't the quickest.
;;;;;;;;;;;;;;;;;;;;;;;

(defun Auto_Play ()
  (let (Game RndSt Move (StepCnt 0))
    (setf RndState (make-random-state t)
          Game (Init_Game RndState))
    (format t "~%~5TStart state ~%~%")
    (Display_Game Game)
    (Auto_Play_Game Game StepCnt)))


; (((GameBoard) (Moves)), ((GameBoard) (Moves)), ...)
(defun Auto_Play_Game (Game Steps)
  (let ((AI_Games (Setup_Starting_Games Game)))
    (print AI_Games)
    (Do_Auto_Play AI_Games Steps)))

(defun Do_Auto_Play (AI_Games Steps)
    (let* ((New_AI_Games (Do_Search AI_Games)) (Winner (Find_Winning_Path New_AI_Games)))
        (if (not (null Winner)) (list Winner Steps)
            (Do_Auto_Play New_AI_Games (1+ Steps))))) 

(defun Setup_Starting_Games (G)
    (list(list (copy-list G) (list '()))))


(defconstant *maxPaths* 100)

(defun Do_Search (AI_Games)
  (let (New_Games)
      (progn
          (dolist (Game AI_Games)
              (dotimes (cnt *elms* New_Games)
                 (if (and (not (Will_Create_Loop (cadr Game) cnt))
                          (Will_Flip_X (car Game) (nth cnt *rules*)))
                     (let ((new_board (Do_Move (car Game) cnt)))
                        (setf New_Games (cons (list new_board (cons cnt (cadr Game))) New_Games))
                     ))
              ))
           (when (> (length New_Games) *maxPaths*) (setf New_Games (Prune New_Games)))
           New_Games)))


(defun Prune (Games)
  ;(print "Pruning")
  (let ((median_length (Get_Median_Path_Len Games)))
    (remove-if #'(lambda (x) (> (length (car x)) median_length)) Games)))

(defun Get_Median_Path_Len (Games)
  (let ((max_len (length (car (first Games)))) (min_len (length (car (first Games)))))
    (dolist (path Games max_len)
      (when (> (length (car path)) max_len) (setf max_len (length (car path))))
      (when (< (length (car path)) min_len) (setf min_len (length (car path)))))
    ;(print max_len)
    ;(print min_len)
    (ceiling (+ max_len min_len) 2.5)))



(defun Will_Flip_X (Game Rule)
 (cond 
  ((null Rule) NIL)
  ((member (car Rule) Game) T)
  (T (Will_Flip_X Game (cdr Rule)))))


(defun Will_Create_Loop (Moves Move &optional (total_moves (cons Move Moves)) (slice_index (1+ (floor (length total_moves) 2))) )
    (let ((l1 (slice total_moves 0 slice_index))
          (l2 (slice total_moves (1+ slice_index) slice_index)))
       ;(print l1)
       ;(print l2)
        (cond
          ((< slice_index 1) NIL)
          ((equal l1 l2) T)
          (T (Will_Create_Loop Moves Move total_moves (1- slice_index))))))
          

(defun get-n-items (lst num)
        (if (> num 0)
            (cons (car lst) (get-n-items (cdr lst) (- num 1)))
            '()))

(defun slice (lst start count)
        (if (> start 1)
            (slice (cdr lst) (- start 1) count)
            (get-n-items lst count)))



(defun Find_Winning_Path (AI_Games)
    (let (Winner)
        (dolist (G AI_Games Winner)
            (if (Check_Win (car G)) 
                (setf Winner (cadr G))))))
