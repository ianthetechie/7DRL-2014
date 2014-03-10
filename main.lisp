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
     ,@forms
     (cl-ncurses:endwin)))


(with-curses-window
  (cl-ncurses:printw "Hello, cl-ncurses")
  (cl-ncurses:refresh)
  (cl-ncurses:getch))
