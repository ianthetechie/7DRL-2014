;; 7DRL - Started 2014-09-03 22:00
;; Written by Ian Wagner


;; ncurses library dependencies
(require :asdf)

(asdf:operate 'asdf:load-op :uffi)

(require 'uffi)

(asdf:oos 'asdf:load-op 'cl-ncurses)

(defmacro with-curses-window (&body forms)
  `(progn
     (cl-ncurses:initscr)
     (cl-ncurses:noecho)  ;; Disable keyboard echo
     ,@forms
     (cl-ncurses:endwin)))

(defun get-keyboard-char ()
  "Reads a single keypress and returns a character literal"
  (code-char (cl-ncurses:getch)))

(defun main ()
  (with-curses-window
    (cl-ncurses:printw "Hello, cl-ncurses")
    (cl-ncurses:refresh)
    (let ((lastkey nil))
      (loop while (not (eql #\Esc lastkey)) do
           ;; Main game input loop
           (setf lastkey (get-keyboard-char))))))

(main)
