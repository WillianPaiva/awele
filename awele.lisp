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

(defconstant north 0)
(defconstant south 1)
(defvar *north-score* 0)
(defvar *south-score* 0)
(defvar *board* '(4 4 4 4 4 4 4 4 4 4 4 4))
(defconstant a 11)
(defconstant b 10)
(defconstant c 9)
(defconstant d 8)
(defconstant e 7)
(defconstant f 6)

(defconstant g 0)
(defconstant h 1)
(defconstant i 2)
(defconstant j 3)
(defconstant k 4)
(defconstant l 5)



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


;; return the opposite player
(defun opponent (player) (if (eql player north) south north))




;;make a move 
(defun make-move (move board player)
  (let ((beans (car (nthcdr move board))))
    (if (zerop beans)
      (error "not a valid move")
      (spread-beans beans (next-house move (1+ move)) move 
                    (replace-nth board move 0))
        )
      )
 )




(defun next-house (move house)
  (if (> house 11)
    (next-house move (- house 12))
    (if (eq move house)
      (next-house move (+ house 1))
      house
      )
  )
 )




(defun sum_beans (player board)
  (if (eq player north)
    (apply '+ (nthcdr 6 board) )
    (apply '+ (subseq board 0 6)))
  )




(defun valid_move (move player board)
  )





(defun player-range (player house)
  (if (and (eq player north) (and (< house 12) (> house 5)))
    t
    (if (and (eq player south ) (and (> house -1) (< house 6)))
      t
      nil)
    )
  )





(defun score-count (house player board)
 )






(defun spread-beans (beans house move board)
  (if (zerop beans)
    board    
    (spread-beans (1- beans) (next-house move (1+ house)) move 
                  (replace-nth board house (+ (car (nthcdr house board)) 1)))
      )
  )
  






(defun print-board (board)  
  (format t "~2&    a    b    c    d    e    f         [North=~2a South=~2a]"
          *north-score* *south-score* )          
  (format t "~%")
  (loop for line from 1 to 6 do
        (format t "    ~d" (car (nthcdr (- 12 line) board)))
        )
  (format t "~%")
  (loop for line from 1 to 6 do
        (format t "    ~d" (car (nthcdr (- line 1) board)))
        )
  (format t "~&    g    h    i    j    k    l  ")   
  )




(defun replace-nth (list n elem)
  (nconc (subseq list 0 n)
         (cons elem (nthcdr (1+ n) list))) 
  )



