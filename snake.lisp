(ql:quickload 'ltk)
(use-package :ltk)

(setf *random-state* (make-random-state t))

(defun zeros (x) (loop for i from 1 to x collect 0))

(defun int-coords (index &optional (maxx 20)) (list (mod index maxx) (floor (/ index maxx))))

(defun grid-coords (index &optional (maxx 20) &aux (coords (int-coords index maxx)))
  (mapcar #'1+ (list (* (car coords) 25) (* (cadr coords) 25)
		     (* (1+ (car coords)) 25) (* (1+ (cadr coords)) 25))))

(defun snake-decf-life (life-vals)
  (loop for i upto (1- (length life-vals))
	unless (zerop i) do (decf (nth i life-vals))
	finally (return life-vals)))

(defun create-grid (canvas life-vals)
  (loop for life in life-vals for i from 0
	collect (apply #'create-rectangle canvas (grid-coords i))))

(defun draw-grid (canvas grid life-vals)
  (loop for life in life-vals for i from 0
	do (itemconfigure canvas (nth i grid) :fill (if (> life 0) 'green 'white))))

(defun index-move (direction) (ccase direction (up -20) (left -1) (right 1) (down 20)))

(defun new-food-index (life-vals)
  (let ((p (loop for life in life-vals for i from 0 if (zerop life) collect i)))
    (nth (random (length p)) p)))

(defun move-food (canvas food old-index life-vals &aux (new-index (new-food-index life-vals)))
  (let ((deltas (mapcar #'- (grid-coords new-index) (grid-coords old-index))))
  (itemmove canvas food (car deltas) (cadr deltas))) new-index)

(defun check-boundaries (index next-move)
  (or (and (< index 20) (equal next-move 'up))
      (and (> index 379) (equal next-move 'down))
      (and (= (mod index 20) 0) (equal next-move 'left))
      (and (= (mod index 20) 19) (equal next-move 'right))))

(defun lose (canvas)
  (create-text canvas 50 50 (nth (random 2) '("Wow, that's really sad." "Well that's just sad."))))

(defun snake (&optional (life-vals (zeros 400)))
  (with-ltk ()
	    (let* ((c (make-instance 'canvas :height 501 :width 501))
		   (grid-field (create-grid c life-vals)) (game-lose nil) (next-move 'right)
		   (index 150) (s-length 5) (food-index (new-food-index life-vals))
		   (food (apply #'create-oval c (grid-coords food-index))))
	      (bind c "<KeyPress-q>"
		    (lambda (evt) (declare (ignore evt))
		      (exit-wish)))
	      (bind c "<KeyPress-r>"
		    (lambda (evt) (declare (ignore evt))
		      (create-grid c life-vals)))
	      (bind c "<KeyPress-Up>"
		    (lambda (evt) (declare (ignore evt))
		      (unless (equal next-move 'down) (setf next-move 'up))))
	      (bind c "<KeyPress-Left>"
		    (lambda (evt) (declare (ignore evt))
		      (unless (equal next-move 'right) (setf next-move 'left))))
	      (bind c "<KeyPress-Right>"
		    (lambda (evt) (declare (ignore evt))
		      (unless (equal next-move 'left) (setf next-move 'right))))
	      (bind c "<KeyPress-Down>"
		    (lambda (evt) (declare (ignore evt))
		      (unless (equal next-move 'up) (setf next-move 'down))))
	      (itemconfigure c food :fill 'red)
	      (pack c)
	      (force-focus c)
	      (loop until game-lose do
		    (progn (process-events)
			   (setf game-lose (check-boundaries index next-move))
			   (incf index (index-move next-move))
			   (if (= index food-index)
			     (progn (incf s-length) 
				   (setf food-index (move-food c food index life-vals)))
			   (setf life-vals (snake-decf-life life-vals)))
			   (process-events)
			   (setf game-lose (> (nth index life-vals) 0))
			   (setf (nth index life-vals) s-length)
			   (draw-grid c grid-field life-vals) 
			   (sleep 1/8))
		    finally (lose c)))))

(snake)
