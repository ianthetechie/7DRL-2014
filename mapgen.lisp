;;;; 7DRL 2014 Map Generator
;;;; Written by Ian Wagner

(defpackage :mapgen
  (:use :common-lisp
        :sb-ext)
  (:export :generate-map
           :iterate-map))
(in-package :mapgen)

(defstruct coord
  row
  col)

(defun generate-grid (height width)
  (make-array (list height width) :initial-element :wall))

(defun floor-tile-percentage (grid)
  (let ((vec (array-storage-vector grid)))
    (/ (count :floor vec) (length vec))))

(defmacro get-tile-value (grid coord)
  `(aref ,grid (coord-row coord) (coord-col coord)))

(defun set-tile-value (grid coord value)
  "Sets the value of the array location (row, col)"
  (setf (get-tile-value grid coord) value))

(defun dig-random-rect-outline (grid coord)
  (set-tile-value grid coord :floor))

(defun dig (grid coord)
  "Dig at a coordinate on the grid"
  (let* ((dig-rect-outline (= (random 3) 0)))
    (if dig-rect-outline
        (if (eql (get-tile-value grid coord) :floor)
            (dig-random-rect-outline grid coord))
        (set-tile-value grid coord :floor))))

(defun generate-map (height width)
  (let ((grid (generate-grid height width))
        (coord (make-coord :row (floor (/ height 2)) :col (floor (/ width 2)))))
    ;; Hollow out the center of the grid
    (set-tile-value grid coord :floor)
    ;; "Walk" around the map and clear out blocks
    (loop while (< (floor-tile-percentage grid) 1/3) do
         ;; Pick a coordinate component (row or col) and a delta (-1 or 1)
         (let* ((state (make-random-state t))
                (row-or-col (if (= (random 2 state) 0) 'coord-row 'coord-col))
                (dimension (if (eql row-or-col 'coord-row)
                               (array-dimension grid 0)
                               (array-dimension grid 1)))
                (delta (cond
                         ((= (funcall row-or-col coord) 3) 1)
                         ((= (funcall row-or-col coord) (- dimension 5)) -1)
                         (t (if (= (random 2 state) 0) 1 -1)))))
           ;; Update the coordinate
           (setf coord (if (eql row-or-col 'coord-row)
                           (make-coord :row (+ (coord-row coord) delta) :col (coord-col coord))
                           (make-coord :row (coord-row coord) :col (+ (coord-col coord) delta))))
           ;; Dig a block
           (dig grid coord)))
    grid))
(defmacro iterate-map (grid row-fcn col-fcn)
  `(loop for row from 0 to (- (array-dimension ,grid 0) 1) do
        (loop for col from 0 to (- (array-dimension ,grid 1) 1) do
             (let ((tile-value (aref ,grid row col)))
               (,row-fcn row col tile-value)))
        (,col-fcn row)))

(defun debug-print-map (grid)
  (iterate-map
   grid
   (lambda (row col val)
     (format t "~c" (cond
                     ((eql val :wall) #\#)
                     ((eql val :floor) #\Space)
                     (t #\?))))
   (lambda (row)
     (format t "~%"))))
