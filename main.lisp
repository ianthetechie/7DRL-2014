;;;; 7DRL - Started 2014-09-03 22:00 EDT
;;;; Written by Ian Wagner


;;; ncurses library dependencies
(require :asdf)

(asdf:operate 'asdf:load-op :uffi)

(require :uffi)

(asdf:oos 'asdf:load-op 'cl-ncurses)

(load "mapgen.lisp")


(defpackage :7DRL-2014
  (:use :common-lisp))
(in-package :7DRL-2014)

(defstruct player
  row
  col
  hp
  max-hp
  level)

(defstruct game-state
  fighting)

(defvar *player* (make-player :hp 10 :max-hp 10 :level 1))
(defvar *game-state* (make-game-state :fighting nil))
(defvar *grid*)


;;; Abstract away common curses window setup code
(defmacro with-curses-window (&body forms)
  `(progn
     (cl-ncurses:initscr)
     (cl-ncurses:noecho)      ; Disable keyboard echo
     (cl-ncurses:curs-set 1)  ; Normal cursor visibility
     ,@forms
     (cl-ncurses:endwin)))

(defun get-keyboard-char ()
  "Reads a single keypress and returns a character literal"
  (code-char (cl-ncurses:getch)))


;;; ncurses drawing functions

(defun draw-rect (row col height width)
  (loop for cur-row from row to (+ height row) do
       (loop for cur-col from col to (+ width col) do
            (cl-ncurses:mvaddch
             cur-row
             cur-col
             (char-code
              (if (or
                   (= cur-row row)
                   (= cur-row (+ height row))
                   (= cur-col col)
                   (= cur-col (+ width col)))
                  #\*
                  #\Space))))))

(defun draw-infobox ()
  (let* ((map-width (mapgen:grid-width *grid*))
             (map-height (mapgen:grid-height *grid*))
             (width (/ map-width 2))
             (height (/ map-height 4))
             (row (- (/ map-height 2) (/ height 2)))
             (col (- (/ map-width 2) (/ width 2))))
        (draw-rect
         (floor row)
         (floor col)
         (floor height)
         (floor width))))

(defun draw-map ()
  (mapgen:iterate-row-major
   *grid*
   (lambda (row col val)
     (cl-ncurses:mvaddch row col
      (char-code (cond
                   ((eql val :wall) #\#)
                   ((eql val :floor) #\Space)
                   (t #\?)))))
   (lambda (row))))

(defun draw-player ()
  (cl-ncurses:mvaddch
   (player-row *player*)
   (player-col *player*)
   (char-code #\@)))

(defun update-display ()
  (cl-ncurses:clear)
  (draw-map)
  (draw-player)
  (cl-ncurses:move (player-row *player*) (player-col *player*))
  (if (game-state-fighting *game-state*)
      (draw-infobox))
  (cl-ncurses:refresh))


;;; Player manipulation functions

(defun set-player-start ()
  "Moves the player to the leftmost, tile of the highest row"
  (mapgen:iterate-col-major
   *grid*
   (lambda (row col val)
     (if (and
          (eq nil (player-row *player*))
          (eql val :floor))
         (progn
           (setf (player-row *player*) row)
           (setf (player-col *player*) col))))
   (lambda (col))))

(defun move-player (key)
  (let ((row-delta 0)
        (col-delta 0))
    (cond
      ((eql key #\h) (setf col-delta -1))
      ((eql key #\l) (setf col-delta 1))
      ((eql key #\k) (setf row-delta -1))
      ((eql key #\j) (setf row-delta 1))
      ((eql key #\u)
       (progn
         (setf row-delta -1)
         (setf col-delta 1)))
      ((eql key #\n)
       (progn
         (setf row-delta 1)
         (setf col-delta 1)))
      ((eql key #\y)
       (progn
         (setf row-delta -1)
         (setf col-delta -1)))
      ((eql key #\b)
       (progn
         (setf row-delta 1)
         (setf col-delta -1))))
    (let ((row (+ (player-row *player*) row-delta))
          (col (+ (player-col *player*) col-delta)))
      (if (eql (mapgen:get-tile-value
                *grid*
                (mapgen:make-coord :row row :col col))
               :floor)
          (progn
            (setf (player-row *player*) row)
            (setf (player-col *player*) col))))))


(defun main ()
  (format t "~%Generating a level...~%")
  (setf *grid* (mapgen:generate-map 59 99))
  (set-player-start)
  (with-curses-window
    (let ((last-key nil))
      (update-display)
      (loop while (not (eql #\Esc last-key)) do
           ;; Main game input loop
           (setf last-key (get-keyboard-char))
           (move-player last-key)
           (update-display)))))

(main)
