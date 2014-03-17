7DRL 2014
=========

My annual 7DRL. Lately I've been learning Lisp via Practical Common Lisp.
I'm only about halfway through the book as of the start of the project, but I
decided to give Lisp a go anyways.

This roguelike is more tactical as opposed to hack and slash, so you should
generally try to creatively evade combat as there are no health potions.

The Plot
--------
You are CLU, a program written by Kevin Flynn. Your task is to locate data to
prove that your user was the original author of Space Paranoids. You must
navigate your way through the fragmented memory canyons of the ENCOM
mainframe, dodging grid bugs and data pushers.

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
![ScreenShot](/Screenshot%20(Final).png)

Disclaimer
----------
This game is provided free of charge under the BSD license. No copyright
infringment is intended by the TRON nomenclature in the above plot text.
I just needed a plot for my game, and thought most geeks would identify
with it ;)
