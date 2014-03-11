;;;; 7DRL 2014 Map Generator
;;;; Written by Ian Wagner

(defpackage :mapgen
  (:use :common-lisp))
(in-package :mapgen)

(defun generate-grid (width height)
  (make-array (list height width) :initial-element 'wall))

(defun floor-tile-percentage (grid)
  (let ((vec (array-storage-vector grid)))
    (/ (count 'floor vec) (length vec))))

(defmacro get-tile-value (grid coord)
  `(aref ,grid (first ,coord) (second ,coord)))

(defun set-tile-value (grid coord value)
  "Sets the value of the array location (row, col)"
  (setf (get-tile-value grid coord) value))

(defun walk (grid coord)
  "Walk to the coordinate on the grid and dig if necessary"
  (set-tile-value grid coord 'floor))

(defun generate-map (size)
  (let ((grid (generate-grid size size))
        (coord (list (floor (/ size 2)) (floor (/ size 2)))))
    ;; Hollow out the center of the grid
    (set-tile-value grid coord 'floor)
    ;; "Walk" around the map and clear out blocks
    (loop while (< (floor-tile-percentage grid) 1/3) do
         ;; Pick a coordinate component (row or col) and a delta (-1 or 1)
         (let* ((row-or-col (if (= (random 2) 0) 'first 'second))
                (delta (cond
                         ((= (funcall row-or-col coord) 1) 1)
                         ((= (funcall row-or-col coord) (- size 2)) -1)
                         (t (if (= (random 2) 0) 1 -1)))))
           ;; Update the coordinate
           (setf coord (if (eql row-or-col 'first)
                           (list (+ (first coord) delta) (second coord))
                           (list (first coord) (+ (second coord) delta))))
           ;; Walk to the coordinte and dig a block
           (walk grid coord)))
    grid))
