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

;;; Abstract away common curses window setup code
(defmacro with-curses-window (&body forms)
  `(progn
     (cl-ncurses:initscr)
     (cl-ncurses:noecho)  ; Disable keyboard echo
     ,@forms
     (cl-ncurses:endwin)))

(defun get-keyboard-char ()
  "Reads a single keypress and returns a character literal"
  (code-char (cl-ncurses:getch)))

(defun draw-map (grid)
  (mapgen:iterate-map
            grid
            (lambda (row col val)
              (cl-ncurses:mvaddch
               row
               col
               (char-code (cond
                            ((eql val :wall) #\#)
                            ((eql val :floor) #\Space)
                            (t #\?)))))
            (lambda (row))))

(defun update-display (grid)
  (cl-ncurses:clear)
  (draw-map grid)
  (cl-ncurses:refresh))

(defun main ()
  (with-curses-window
    (let ((lastkey nil)
          (grid (mapgen:generate-map 24 80)))
      (update-display grid)
      (loop while (not (eql #\Esc lastkey)) do
           ;; Main game input loop
           (setf lastkey (get-keyboard-char))
           (update-display grid)))))

(main)
