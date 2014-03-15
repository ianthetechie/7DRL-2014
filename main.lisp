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
  recognized
  finished)

(defstruct adversary
  type
  hp
  level)

(defvar *player-color-pair* 1)
(defvar *objective-color-pair* 2)

(defvar *player* (make-player :hp 10 :max-hp 10 :level 1))
(defvar *enemy*)
(defvar *game-state* (make-game-state :recognized nil :finished nil))
(defvar *grid*)


;;; Abstract away common curses window setup code
(defmacro with-curses-window (&body forms)
  `(progn
     (cl-ncurses:initscr)
     (cl-ncurses:noecho)      ; Disable keyboard echo
     (cl-ncurses:curs-set 1)  ; Normal cursor visibility

     ;; Set up colors
     (cl-ncurses:start-color)

     (cl-ncurses:init-pair
      *player-color-pair*
      cl-ncurses:color_yellow
      cl-ncurses:color_white)

     (cl-ncurses:init-pair
      *objective-color-pair*
      cl-ncurses:color_green
      cl-ncurses:color_black)
     
     ;; Code body
     ,@forms
     
     ;; Cleanup
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

(defun draw-string (row col str)
  (loop
     for c across str
     for i from 0 to (length str)
     do
       (cl-ncurses:mvaddch
        (floor row)
        (floor (+ col i))
        (char-code c))))

(defun draw-infobox (title)
  (let* ((map-width (mapgen:grid-width *grid*))
             (map-height (mapgen:grid-height *grid*))
             (width (floor (/ map-width 2)))
             (height (floor (/ map-height 4)))
             (row (floor (- (/ map-height 2) (/ height 2))))
             (col (floor (- (/ map-width 2) (/ width 2)))))
    (draw-rect row col height width)
    (draw-string (+ row 2) (+ col 2) title)))

(defun draw-map ()
  (mapgen:iterate-row-major
   *grid*
   (lambda (row col val)
     (if (eql val :objective)
         (cl-ncurses:attron (cl-ncurses:color-pair *objective-color-pair*)))
     (cl-ncurses:mvaddch row col
      (char-code (cond
                   ((eql val :wall) #\#)
                   ((eql val :floor) #\Space)
                   ((eql val :objective) #\!)
                   (t #\?))))
     (cl-ncurses:attroff (cl-ncurses:color-pair *objective-color-pair*)))
   (lambda (row))))

(defun draw-player ()
  (cl-ncurses:attron (cl-ncurses:color-pair *player-color-pair*))
  (cl-ncurses:mvaddch
   (player-row *player*)
   (player-col *player*)
   (char-code #\@))
  (cl-ncurses:attroff (cl-ncurses:color-pair *player-color-pair*)))

(defun update-display ()
  (cl-ncurses:clear)
  (draw-map)
  (draw-player)
  (cl-ncurses:move (player-row *player*) (player-col *player*))
  (cond
    ((game-state-recognized *game-state*)
     (draw-infobox "You've been spotted by a recognizer!"))
    ((game-state-finished *game-state*)
     (draw-infobox "Congratulations on retrieving the data. END OF LINE.")))
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

(defun do-player-action (key)
  (let ((row-delta 0)
        (col-delta 0))
    (if (cond
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
        (let* ((row (+ (player-row *player*) row-delta))
               (col (+ (player-col *player*) col-delta))
               (tile (mapgen:get-tile-value
                      *grid*
                      (mapgen:make-coord :row row :col col))))
          (cond
            ((eql tile :floor)
             (progn
               (setf (player-row *player*) row)
               (setf (player-col *player*) col)))
            ((eql tile :objective)
             (progn
               (setf (player-row *player*) row)
               (setf (player-col *player*) col)
               (setf (game-state-finished *game-state*) t))))))))
            
(defun do-fight-action (key)
  (cond
    ((eql key #\Space) (setf (game-state-recognized *game-state*) nil))))

(defun main ()
  (format t "~%Fragmenting the memory canyons...~%")
  (setf *grid* (mapgen:generate-map 75 271))
  (set-player-start)
  (with-curses-window
    (let ((last-key nil)
          (should-quit nil)
          (state (make-random-state t)))
      (update-display)
      (loop while (not (or
                        (eql #\Esc last-key)
                        should-quit)) do
           ;; Main game input loop
           (setf should-quit (game-state-finished *game-state*))
           (setf last-key (get-keyboard-char))
           (if (game-state-recognized *game-state*)
               (do-fight-action last-key)  ; Special input handler for menu-based combat
               (progn                      ; Normal movement
                 (if (and                  ; If the player moves, there is a chance to be recognized
                      (do-player-action last-key)
                      (= (random 100 state) 0))
                     (setf (game-state-recognized *game-state*) t))))
           (update-display)))))

(main)
