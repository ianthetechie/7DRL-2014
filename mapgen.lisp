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
  `(aref ,grid (coord-row ,coord) (coord-col ,coord)))

(defun set-tile-value (grid coord value)
  "Sets the value of the array location (row, col)"
  (setf (get-tile-value grid coord) value))

(defun is-near-n-walls (grid coord num-walls range)
  (let ((row (coord-row coord))
        (col (coord-col coord))
        (near-walls 0))
    (loop for row-delta from (- range) to range do
         (loop for col-delta from (- range) to range do
              (if (and
                   (not (= row-delta col-delta 0))
                   (eql
                    (get-tile-value grid (make-coord
                                          :row (+ row row-delta)
                                          :col (+ col col-delta)))
                    :wall))
                  (incf near-walls))))
    (>= near-walls num-walls)))

(defun ortho-step (grid coord state)
  (let* ((row-or-col (if (= (random 2 state) 0) 'coord-row 'coord-col))
         (dimension (if (eql row-or-col 'coord-row)
                        (array-dimension grid 0)
                        (array-dimension grid 1)))
         (delta (cond
                  ((= (funcall row-or-col coord) 3) 1)
                  ((= (funcall row-or-col coord) (- dimension 4)) -1)
                  (t (if (= (random 2 state) 0) 1 -1)))))
    (if (eql row-or-col 'coord-row)
        (make-coord :row (+ (coord-row coord) delta) :col (coord-col coord))
        (make-coord :row (coord-row coord) :col (+ (coord-col coord) delta)))))

(defun dig-other-shape (grid coord state)
  (let ((second-coord (ortho-step grid coord state)))
    (set-tile-value grid coord :floor)
    (set-tile-value grid second-coord :floor)))
;    (set-tile-value grid (ortho-step grid second-coord state) :floor)))
                    
(defun dig (grid coord state)
  "Dig at a coordinate on the grid"
  (let ((dig-rect-outline (= (random 6 state) 0)))
    (if dig-rect-outline
        (if (eql (get-tile-value grid coord) :wall)
            (dig-other-shape grid coord state))
        (set-tile-value grid coord :floor))))

(defun generate-map (height width)
  (let ((grid (generate-grid height width))
        (coord (make-coord :row (floor (/ height 2)) :col (floor (/ width 2))))
        (state (make-random-state t)))
    ;; Hollow out the center of the grid
    (set-tile-value grid coord :floor)
    ;; "Walk" around the map and clear out blocks
    (loop while (< (floor-tile-percentage grid) 1/3) do
         ;; Pick a coordinate component (row or col) and a delta (-1 or 1)
         (let ((new-coord (ortho-step grid coord state)))
           (loop while (not (is-near-n-walls grid new-coord 14 2)) do
                (setf new-coord (ortho-step grid new-coord state)))
           (setf coord new-coord)
           (dig grid coord state)))
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
