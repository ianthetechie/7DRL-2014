7DRL 2014
=========

My annual 7DRL. Lately I've been learning Lisp via Practical Common Lisp.
I'm only about halfway through the book as of the start of the project, but I
decided to give Lisp a go anyways. Should be fun!

Dependencies
------------
All deveopment was done with SBCL, and the sb-ext library is used for fetching 2D array dimensions at runtime.

```lisp
(require 'asdf)
(require 'asdf-install)
(asdf-install:install 'cl-ncurses)
```

Screenshot
----------
![ScreenShot](/Screenshot%20(Day%207).png)
