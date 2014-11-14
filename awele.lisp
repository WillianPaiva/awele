;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Copyright (C) Fri 12 Sep 2014 03:00:35 PM CEST Author Willian Paiva                ;;;
;;;                                                                                     ;;;
;;;  This program is free software: you can redistribute it and/or modify               ;;;
;;;  it under the terms of the GNU General Public License as published by               ;;;
;;;  the Free Software Foundation, either version 3 of the License, or                  ;;;
;;;  (at your option) any later version.                                                ;;;
;;;                                                                                     ;;;
;;;  This program is distributed in the hope that it will be useful,                    ;;;
;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of                     ;;;
;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                      ;;;
;;;  GNU General Public License for more details.                                       ;;;
;;;                                                                                     ;;;
;;;  You should have received a copy of the GNU General Public License                  ;;;
;;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                     player definitions                            ;;;{{{ 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant north 0)
(defconstant south 1)
(defvar *ai* north)


;;;}}}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                        board definitions                          ;;;{{{ 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *north-score* 0)
(defvar *south-score* 0)
(defvar *board* '(4 4 4 4 4 4 4 4 4 4 4 4))


;; reset the game to initial stage


(defun reset-game ()
    (setq *north-score* 0)
    (setq *south-score* 0)
    (setq *board* '(4 4 4 4 4 4 4 4 4 4 4 4)))

;;;}}}






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                      main-standalone (case)                       ;;;{{{
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;takes a move axecute and return ai move it parameter nil execute just the ai move and return it 
;; case a move is passed as a parameter execute player move and followed by ai move and return it
;; attention as this function uses variables to hold the board and score states it need to be reset before starting a function ---> game reset-game


(defun main-standalone (x)
  (let ((move nil)
        (t-n-score 0)
        (t-s-score 0))
    (if (not (equal x nil))
      (if (= *ai* north)
        (progn 
          (multiple-value-setq (*board* t-s-score) (make-move x *board* south))
          (setq *south-score* (+ *south-score* t-s-score))
          (setq t-s-score 0)
          (setq move (ai-strategy north *board* *north-score* *south-score*))
          (multiple-value-setq (*board* t-n-score) (make-move move *board* north))
          (setq *north-score* (+ *north-score* t-n-score))
          (setq t-n-score 0))
        (progn 
          (multiple-value-setq (*board* t-n-score) (make-move x *board* north))
          (setq *north-score* (+ *north-score* t-n-score))
          (setq t-n-score 0)
          (setq move (ai-strategy south  *board* *north-score* *south-score*))
          (multiple-value-setq (*board* t-s-score) (make-move move *board* south))
          (setq *south-score* (+ *south-score* t-s-score))
          (setq t-s-score 0)))
      (if (= *ai* north)
        (progn
          (setq move (ai-strategy north *board* *north-score* *south-score*))
          (multiple-value-setq (*board* t-n-score) (make-move move *board* north))
          (setq *north-score* (+ *north-score* t-n-score))
          (setq t-n-score 0))
        (progn 
          (setq move (ai-strategy south  *board* *north-score* *south-score*))
          (multiple-value-setq (*board* t-s-score) (make-move move *board* south))
          (setq *south-score* (+ *south-score* t-s-score))
          (setq t-s-score 0))))
    move))




;;; }}} ;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                            game start                             ;;;{{{
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; start the axele game playing human vs ai 


(defun awele ()
    (if (= *ai* north)
      (game-loop #'ai-strategy #'human-strategi)
      (game-loop #'human-strategi #'ai-strategy)))


;;; }}} ;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                           init-standalone                         ;;;{{{ 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; define were ai play as north ore south 

(defun init-standalone (x)
  (reset-game)
  (if (not (equal x nil))
      (setq *ai* north)
      (setq *ai* south)))

;;; }}} ;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                      return opposite player                       ;;;{{{
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;takes a player and return the opponent

(defun opponent (player) (if (eql player north) south north))


;;; }}} ;;; 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                              game loop                             ;;;{{{
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;starts the game loop north is always the first to play 

(defun game-loop (north-strategi south-strategi)
  (reset-game)
  (let ((board *board*)
        (player north)
        (n-score 0)
        (s-score 0)
        (t-n-score 0)
        (t-s-score 0)) 
    (loop while (not (game-over player board n-score s-score))
          do (progn
               (print-board board n-score s-score)
               (if (= player north)
                 (multiple-value-setq (board t-n-score) (turn board north-strategi north n-score s-score))
                 (multiple-value-setq (board t-s-score) (turn board south-strategi south n-score s-score)))
               (setq player (opponent player))
               (setq n-score (+ t-n-score n-score))
               (setq s-score (+ t-s-score s-score))
               (setq t-n-score 0)
               (setq t-s-score 0)))
    (if (and (> 25 n-score) (> 25 s-score))
      (progn 
        (setq n-score (+ n-score (sum_beans north board)))
        (setq s-score (+ s-score (sum_beans south board)))
        (setq board '(0 0 0 0 0 0 0 0 0 0 0 0))
        (print-board board n-score s-score)
        )
      (print-board board n-score s-score))
  (if (= n-score s-score)
    2
    (if (> n-score s-score)
      0
      1))))





(defun test (&optional (nor 0) (draw 0) (sou 0))
  (dotimes (x 1000) 
    (let ((result (game-loop #'ai-strategy #'randon-stratey)))
      (format t "---> ~d" x)
      (if (= result 2)
        (setq draw (1+ draw))
        (if (= result 0)
          (setq nor (1+ nor))
          (setq sou (1+ sou))))))
  (list draw nor sou))
;;; }}} ;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                               turn                                ;;;{{{
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; makes the play turn gettind the move from the proper strategy 
;; returns modified board and player new score

(defun turn (board strategi player n-score s-score)
  ;(format t "turn ~d" player)
  (make-move (funcall strategi player board n-score s-score) board player))
;;; }}} ;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                               make move                             ;;;{{{
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; makes the player move  
;; returns modified board and player new score

(defun make-move (move board player)
  ;(format t "make move ~d" player)
  (if move
    (progn 
      (let ((beans (car (nthcdr move board))))
        (if (valid_move move player board)
          (spread-beans beans (next-house move (1+ move)) move 
                        (replace-nth board move 0) player)
          (error "not a valid move"))))
    (values board 0)))
;;; }}} ;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                           human strategi                           ;;;{{{
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;sent a prompt to the human player presenting all possible moves and return player choice

(defun human-strategi (player board n-score s-score)
  (let ((li (valid-list player board)))
    (format t "valid moves -->")
    (loop for line from 0 to (1-(length li)) do
          (format t "    ~d" (car (nthcdr line li))))
    (format t "~%")
    (format t "please enter your move ~%")
    (let ((move (read)))
      (if (member move li)
        move
        (progn
          (format t "INVALID MOVE ~%")
          (human-strategi player board n-score s-score))))))

;;; }}} ;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                          random strategi                          ;;;{{{
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;return a random move based on the possible moves for a player

(defun randon-stratey (player board n-score s-score)
  (declare (ignore n-score) (ignore s-score))
  (let ((li (valid-list player board)))
    (if (listp li)
      (nth (random (length li)) li)
      (error "bad move"))))



;;; }}} ;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;                            alpha-beta                             ;;;{{{
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; return a move based on a alpha-beta algorthim 

(defun ai-strategy (player board n-score s-score)
  (multiple-value-bind (value move)
    (alpha-beta player board n-score s-score -200 200 6) 
    (declare (ignore value))
    move))

(defun alpha-beta (player board n-score s-score alpha beta depth)
  (if (= depth 0) 
    (eval-s player n-score s-score)
    (let* ((moves (valid-list player board))
           (b-move (if (endp moves) nil (first moves))))
      (loop for move in moves do
            (let* ((l-board-score (multiple-value-list (make-move move (copy-seq board) player)))
                   (board2 (car l-board-score))
                   (val (- (alpha-beta (opponent player) board2 
                                       (if (= player north ) (+ n-score (cadr l-board-score)) n-score)
                                       (if (= player south ) (+ s-score (cadr l-board-score)) s-score)
                                       (- beta) (- alpha) 
                                       (1- depth)))))
              (when (> val alpha)
                (setf alpha val)
                (setf b-move move)))
            until (>= alpha beta))
      (values alpha b-move))))





(defun eval-s (player n-score s-score)
  (if (= player north)
    (if (>= n-score 24) (- (+ 100 n-score) s-score) (- n-score s-score))
    (if (>= s-score 24) (- (+ 100 s-score) n-score) (- s-score n-score))))


;;; }}} ;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                               game-over                             ;;;{{{
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; return if game is over

(defun game-over (player board n-score s-score )
  (if (or (or (> n-score 24) (> s-score 24)) (endp (valid-list player board)))
    t
    nil)
  )
;;; }}} ;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                  return next house for the move                   ;;;{{{
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;computes the next house followin the rules of the game 


(defun next-house (move house)
  (if (> house 11)
    (next-house move (- house 12))
    (if (eq move house)
      (next-house move (+ house 1))
      house
      )
    ))



(defun get-last (beans move &optional (house move))
  (if (zerop beans) 
    house
    (get-last (1- beans) move (next-house move (1+ house)))))

;;; }}} ;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                 sum_beans and get-beans                           ;;;{{{
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; return the sum of the beans of a player 

(defun sum_beans (player board)
  (if (eq player south)
    (apply '+ (nthcdr 6 board) )
    (apply '+ (subseq board 0 6))))

;; get the beans of a given house

(defun get-beans (house board)
  (car (nthcdr house board)))


;;; }}} ;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                     return if move is  valid                       ;;;{{{
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;define if a move is valid or not
(defun valid_move (move player board)
  ;(format t "valid move ~d" player)
  (and
    (and 
      (> (get-beans move board) 0) 
      (player-range player move)) 
    (feed-opponent move player board))) 

;;; }}} ;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;             return list of valid  moves for a player               ;;;{{{
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;make a list with valid list of moves da a player 
(defun valid-list (player board &optional (valid '()) (cn 5) (cs 11))
  (if (or (= cn -1) (= cs 5))
    valid
    (if (= player south)
      (valid-list player board (if (valid_move cs player board) (cons cs valid) valid)  cn (1- cs))
      (valid-list player board (if (valid_move cn player board) (cons cn valid) valid) (1- cn) cs))))


;;; }}} ;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                            feed oponent                            ;;;{{{
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;);;;;;;;;;;;;;;;;;;


;;check if a move feeds the oponent 
(defun feed-opponent (move player board)
  (if (> (sum_beans (opponent player) board) 0)
    t
    (if (and (> move -1) (< move 6))
      (> (+ (get-beans move board) move) 5)
      (> (+ (get-beans move board) move) 11))))


;;; }}} ;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              return if a house is in the player range              ;;;{{{
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun player-range (player house)
  ;(format t "player range ~d   house --> ~d" player house)
  (if (and (eq player south) (and (< house 12) (> house 5)))
    t
    (if (and (eq player north) (and (> house -1) (< house 6)))
      t
      nil)
    ))

;;; }}} ;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   spread the beans after a move                   ;;;{{{
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;spread the beans and return a a board and score 


(defun spread-beans (beans house move board player)
  ;(format t "spread ~d" player)
  (if (zerop beans)
    (score board (if (eq house 0) 11 (1- house)) player)    
    (spread-beans (1- beans) (next-house move (1+ house)) move 
                  (replace-nth board house (+ (car (nthcdr house board)) 1)) player)))

;;; }}} ;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                            update score                            ;;;{{{
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; update the player score after a move 

(defun score (board house player &optional (score 0))
  ;(format t "score ~d" player)
  (if (or (player-range player house) (< house 0))
    (values board score)
    (if (or (eq (get-beans house board) 2) 
            (eq (get-beans house board) 3) )
      (score (replace-nth board house 0) (1- house) player (+ score (get-beans house board)))
      (values board score))))
;;; }}} ;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;               print the board with the score                   ;;;{{{
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defun print-board (board n-score s-score)  
  (format t "~%")
  (format t "~%")
  (format t "===========================================~%")
  (format t "~&|  c5  |  c4  |  c3  |  c2  |  c1  |  c0  |       [North=~2a South=~2a] ~%"
          n-score s-score )
  (format t "===========================================~%")
  (format t "|")
  (loop for line from 1 to 6 do
        (format t "  ~2d  |" (car (nthcdr (- 6 line) board)))
        )
  (format t "~%")
  (format t "-------------------------------------------~%")
  (format t "|")
  (loop for line from 1 to 6 do
        (format t "  ~2d  |" (car (nthcdr (+ line 5) board)))
        )
  (format t "~%")
  (format t "===========================================~%")
  (format t "~&|  c6  |  c7  |  c8  |  c9  |  c10 |  c11 |~%")   
  (format t "===========================================~%")
  )

;;; }}} ;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;           return a list with the nth element replacede            ;;;{{{
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun replace-nth (list n elem)
  (nconc (subseq list 0 n)
         (cons elem (nthcdr (1+ n) list))) 
  )

;;; }}} ;;;
