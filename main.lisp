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
  attack
  def)

(defstruct game-state
  recognized
  finished)

(defstruct baddie
  row
  col
  type
  hp
  attack
  def)

(defvar *default-color-pair* 0)
(defvar *player-color-pair* 1)
(defvar *objective-color-pair* 2)
(defvar *baddie-color-pair* 3)

(defvar *player* (make-player :hp 10 :max-hp 10 :attack 5 :def 5))
(defvar *enemy*)
(defvar *game-state* (make-game-state :recognized nil :finished nil))
(defvar *grid*)
(defvar *baddies* (make-array 20 :fill-pointer 0 :adjustable t))


;;; Abstract away common curses window setup code
(defmacro with-curses-window (&body forms)
  `(progn
     (cl-ncurses:initscr)
     (cl-ncurses:noecho)      ; Disable keyboard echo
     (cl-ncurses:curs-set 1)  ; Normal cursor visibility

     ;; Set up colors
     (cl-ncurses:start-color)

     (cl-ncurses:init-pair
      *default-color-pair*
      cl-ncurses:color_white
      cl-ncurses:color_black)
     
     (cl-ncurses:init-pair
      *player-color-pair*
      cl-ncurses:color_yellow
      cl-ncurses:color_white)

     (cl-ncurses:init-pair
      *objective-color-pair*
      cl-ncurses:color_green
      cl-ncurses:color_black)

     (cl-ncurses:init-pair
      *baddie-color-pair*
      cl-ncurses:color_red
      cl-ncurses:color_black)
     
     ;; Code body
     ,@forms
     
     ;; Cleanup
     (cl-ncurses:endwin)))

(defun mvaddch-with-color-pair (y x ch color-pair)
  "Wraps a call to mvaddch with enable and disable color pair calls"
  (cl-ncurses:attron (cl-ncurses:color-pair color-pair))
  (cl-ncurses:mvaddch y x ch)
  (cl-ncurses:attroff (cl-ncurses:color-pair *baddie-color-pair*)))

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
     (let* ((tile-info (cond
                         ((eql val :wall)
                          (list #\# *default-color-pair*))
                         ((eql val :floor)
                          (list #\Space *default-color-pair*))
                         ((eql val :objective)
                          (list #\! *objective-color-pair*))
                         (t
                          (list #\? *default-color-pair*))))
            (ch (char-code (first tile-info)))
            (color-pair (second tile-info)))
       (mvaddch-with-color-pair
        row
        col
        ch
        color-pair)))
   (lambda (row))))

(defun draw-baddies ()
  (loop for baddie across *baddies* do
       (mvaddch-with-color-pair
        (baddie-row baddie)
        (baddie-col baddie)
        (char-code
         (cond
           ((eql (baddie-type baddie) :grid-bug)
            #\g)
           (t #\?)))
        *baddie-color-pair*)))

(defun draw-player ()
  (mvaddch-with-color-pair
   (player-row *player*)
   (player-col *player*)
   (char-code #\@)
   *player-color-pair*))

(defun update-display ()
  (cl-ncurses:clear)
  (draw-map)
  (draw-baddies)
  (draw-player)
  (cl-ncurses:move (player-row *player*) (player-col *player*))
  (cond
    ((game-state-recognized *game-state*)
     (draw-infobox "You've been spotted by a recognizer!"))
    ((game-state-finished *game-state*)
     (draw-infobox "Congratulations on retrieving the data. END OF LINE.")))
  (cl-ncurses:refresh))

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


;;; Spawn some grid bugs and crap!
(defun spawn-baddies (state)
  (mapgen:iterate-row-major
   *grid*
   (lambda (row col val)
     (if (and
          (eql val :floor)
          (= (random 200 state) 0))
         (vector-push-extend
          (make-baddie
           :row row
           :col col
           :type :grid-bug
           :hp 5
           :attack 5
           :def 5)
          *baddies*)))
   (lambda (col))))

(defun distance (x1 y1 x2 y2)
  (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2))))

(defun compare (a b)
  (cond
    ((< a b) 1)
    ((> a b) -1)
    (t 0)))

(defun move-baddies (state)
  (loop for baddie across *baddies* do
       ;; Check distance from player, and try to move closer if possible
       (let ((col-baddie (baddie-col baddie))
             (row-baddie (baddie-row baddie))
             (col-player (player-col *player*))
             (row-player (player-row *player*))
             (trigger-distance 4))
         (if (<= (distance col-baddie row-baddie col-player row-player) trigger-distance)
             (let* ((row-delta (compare row-baddie row-player))
                    (col-delta (compare col-baddie col-player))
                    (coord (mapgen:make-coord
                            :row (+ row-baddie row-delta)
                            :col (+ col-baddie col-delta))))
               (if (and
                    (eql :floor (mapgen:get-tile-value *grid* coord))
                    (> (random 4) 0))  ; 25% chance to fail move
                   (progn
                     (setf (baddie-row baddie) (mapgen:coord-row coord))
                     (setf (baddie-col baddie) (mapgen:coord-col coord)))))))
       ;; Otherwise, there is a chance to move in a random direction
       (if (= (random 3 state) 0)
           (let* ((row (baddie-row baddie))
                  (col (baddie-col baddie))
                  (row-delta (- 1 (random 3 state)))
                  (col-delta (- 1 (random 3 state)))
                  (coord (mapgen:make-coord
                          :row (+ row row-delta)
                          :col (+ col col-delta))))
             (if (eql :floor (mapgen:get-tile-value *grid* coord))
                 (progn
                   (setf (baddie-row baddie) (mapgen:coord-row coord))
                   (setf (baddie-col baddie) (mapgen:coord-col coord))))))))
             

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
  (let ((state (make-random-state t)))
    (setf *grid* (mapgen:generate-map 75 271 state))
    (set-player-start)
    (spawn-baddies state)
    (with-curses-window
        (let ((last-key nil)
              (should-quit nil))
          (update-display)
          (loop while (not (or
                            (eql #\Esc last-key)
                            should-quit)) do
             ;; Main game input loop
               (setf should-quit (game-state-finished *game-state*))
               (setf last-key (get-keyboard-char))
               (if (game-state-recognized *game-state*)
                   (do-fight-action last-key)  ; Special input handler for menu-based combat
                                               ; Normal movement
                   (if (do-player-action last-key)
                       (progn
                         (move-baddies state)  ; If the player moves, there is a chance to be recognized
                         (if (= (random 100 state) 0)
                             (setf (game-state-recognized *game-state*) t)))))
               (update-display))))))

(main)
