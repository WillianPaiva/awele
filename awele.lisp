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
;;;                     playe r definitions                            ;;;{{{
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant north 0)
(defconstant south 1)
(defvar *north-score* 0)
(defvar *south-score* 0)
(defvar *t-north-score* 0)
(defvar *t-south-score* 0)
(defvar *normal* t)


;;;}}}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                        board definitions                          ;;;{{{
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar *board* '(4 4 4 4 4 4 4 4 4 4 4 4))
;;;}}}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                           upper board                             ;;;{{{
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defconstant a 11)
(defconstant b 10)
(defconstant c 9)
(defconstant d 8)
(defconstant e 7)
(defconstant f 6)

;;; }}} ;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                           lower board                             ;;;{{{
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant g 0)
(defconstant h 1)
(defconstant i 2)
(defconstant j 3)
(defconstant k 4)
(defconstant l 5)

;;; }}} ;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                      reset board and scores                       ;;;{{{
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun reset-game ()
  (reset-score)
  (starting_board)
  )



(defun reset-score ()
  (setf *north-score* 0)
  (setf *south-score* 0)
  )




(defun starting_board ()
  (setf *board* '(4 4 4 4 4 4 4 4 4 4 4 4))
  )


;;; }}} ;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                      re turn opposite player                       ;;;{{{
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun opponent (player) (if (eql player north) south north))


;;; }}} ;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                              make move                             ;;;{{{
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-move (move board player)
  ;(format t "make move ~d" player)
  (let ((beans (car (nthcdr move board))))
    (if (valid_move move player board)
      (spread-beans beans (next-house move (1+ move)) move 
                    (replace-nth board move 0) player)
      (error "not a valid move"))))
;;; }}} ;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                             game loop                             ;;;{{{
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun game-loop (north-strategi south-strategi)
  (let ((board *board*)
        (player north)
        (n-score 0)
        (s-score 0)
        (t-n-score 0)
        (t-s-score 0)) 
    (reset-game)
    (loop while (not (game-over player board n-score s-score))
          do (progn
               (print-board board n-score s-score)
               (if (= player north)
                 (multiple-value-setq (board t-n-score) (turn board north-strategi north))
                 (multiple-value-setq (board t-s-score) (turn board south-strategi south)))
               (setq player (opponent player))
               (setq n-score (+ t-n-score n-score))
               (setq s-score (+ t-s-score s-score))))
    (print-board board n-score s-score)))

<<<<<<< HEAD
(defmethod turn (board strategi player)
  ;(format t "turn ~d" player)
  (make-move (funcall strategi player board) board player))
;;; }}} ;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                          human strategi                           ;;;{{{
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun human-strategi (player board)
  (let ((li (valid-list player board)))
    (format t "valid moves -->")
    (loop for line from 0 to (1-(length li)) do
          (format t "    ~d" (car (nthcdr line li))))
    (format t "~%")
    (format t "please enter your move ~%")
    (read)))

;;; }}} ;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                          random strategi                          ;;;{{{
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun randon-stratey (player board)
  (let ((li (valid-list player board)))
    (if (listp li)
      (nth (random (length li)) li)
      (error "bad move"))))



;;; }}} ;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;                            alpha-beta                             ;;;{{{
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(defun alpha-beta (player board)
;)





;;; }}} ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                              game-over                             ;;;{{{
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun game-over (player board n-score s-score )
  (if (or (or (> n-score 24) (> s-score 24)) (endp (valid-list player board)))
    t
    nil)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                  return next house for the move                   ;;;{{{
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun next-house (move house)
  (if (> house 11)
    (next-house move (- house 12))
    (if (eq move house)
      (next-house move (+ house 1))
      house
      )
    )



(defun get-last (beans move &optional (house move))
  (if (zerop beans) 
    house
    (get-last (1- beans) move (next-house move (1+ house)))))

;;; }}} ;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                 return sum of beans for a player                  ;;;{{{
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun sum_beans (player board)
  (if (eq player south)
    (apply '+ (nthcdr 6 board) )
    (apply '+ (subseq board 0 6))))

(defun set-player-score (player n)
  (if (eql player north)
    (setq *north-score* (+ *north-score* n))
    (setq *south-score* (+ *south-score* n))
    ))




;;; }}} ;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                     return if move is valid                       ;;;{{{
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun valid_move (move player board)
    ;(format t "valid move ~d" player)
    (and 
      (> (get-beans move board) 0) 
      (player-range player move)) 
    (feed-opponent move player board))) 

;;; }}} ;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;             return list of valid  moves for a player               ;;;{{{
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun valid-list (player board &optional (valid '()) (cn 5) (cs 11))
  (if (or (= cn -1) (= cs 5))
    valid
    (if (= player south)
      (valid-list player board (if (valid_move cs player board) (cons cs valid) valid)  cn (1- cs))
      (valid-list player board (if (valid_move cn player board) (cons cn valid) valid) (1- cn) cs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                            feed oponent                            ;;;{{{
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;);;;;;;;;;;;;;;;;;;

(defmethod feed-opponent (move player board)
  (if (> (sum_beans (opponent player) board) 0)
    t
    (if (and (> move -1) (< move 6))
      (> (+ (get-beans move board) move) 5)
      (> (+ (get-beans move board) move) 11))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;             return if a house is in the player range              ;;;{{{
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun player-range (player house)
  ;(format t "player range ~d   house --> ~d" player house)
  (if (and (eq player south) (and (< house 12) (> house 5)))
    t
    (if (and (eq player north) (and (> house -1) (< house 6)))
      t
      nil)
    )

;;; }}} ;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   spread the beans after a move                   ;;;{{{
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun spread-beans (beans house move board player)
  ;(format t "spread ~d" player)
  (if (zerop beans)
    (score board (if (eq house 0) 11 (1- house)) player)    
    (spread-beans (1- beans) (next-house move (1+ house)) move 
                  (replace-nth board house (+ (car (nthcdr house board)) 1)) player)))

;;; }}} ;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                           update score                            ;;;{{{
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun score (board house player &optional (score 0))
  ;(format t "score ~d" player)
  (if (or (player-range player house) (< house 0))
    (values board score)
    (if (or (eq (get-beans house board) 2) 
            (eq (get-beans house board) 3) )
      (score (replace-nth board house 0) (1- house) player (+ score (get-beans house board)))
      (values board score))));;; }}} ;;;

;;; }}} ;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                  print the board with the score                   ;;;{{{
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defun print-board (board n-score s-score)  
  (format t "~2&    c5    c4    c3    c2    c1    c0         [North=~2a South=~2a]"
          n-score s-score )          
  (format t "~%")
  (loop for line from 1 to 6 do
        (format t "    ~2d" (car (nthcdr (- 6 line) board)))
        )
  (format t "~%")
  (loop for line from 1 to 6 do
        (format t "    ~2d" (car (nthcdr (+ line 5) board)))
        )
  (format t "~&    c6    c7    c8    c9    c10    c11  ~%")   
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
