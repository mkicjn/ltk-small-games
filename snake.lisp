(ql:quickload 'ltk)
(use-package :ltk)

(setf *random-state* (make-random-state t))

(defun zeros (n) (loop repeat n collect 0))

(defun int-coords (index &optional (maxx 20)) (list (mod index maxx) (floor (/ index maxx))))

(defun grid-coords (index &optional (maxx 20) &aux (coords (int-coords index maxx)))
  (mapcar #'1+ (list (* (car coords) 25) (* (cadr coords) 25)
		     (* (1+ (car coords)) 25) (* (1+ (cadr coords)) 25))))

(defun update-life (life-vals)
  (mapcar (lambda (x) (if (zerop x) 0 (funcall (if (> x 0) #'1- #'1+) x))) life-vals))

(defun create-grid (canvas life-vals)
  (loop for life in life-vals for i from 0
	collect (apply #'create-rectangle canvas (grid-coords i))))

(defun draw-grid (canvas grid life-vals)
  (loop for life in life-vals for i from 0
	do (itemconfigure canvas (nth i grid) :fill (if (zerop life) 'white
						      (if (> life 0) 'green 'blue)))))

(defun index-move (direction) (ccase direction (up -20) (left -1) (right 1) (down 20)))

(defun random-elt (l) (nth (random (length l)) l))

(defun new-food-index (life-vals)
  (let ((p (loop for life in life-vals for i from 0 if (zerop life) collect i))) (random-elt p)))

(defun move-food (canvas food old-index life-vals &aux (new-index (new-food-index life-vals)))
  (let ((deltas (mapcar #'- (grid-coords new-index) (grid-coords old-index))))
  (itemmove canvas food (car deltas) (cadr deltas))) new-index)

(defun check-boundaries (index next-move)
  (or (and (< index 20) (equal next-move 'up))
      (and (> index 379) (equal next-move 'down))
      (and (= (mod index 20) 0) (equal next-move 'left))
      (and (= (mod index 20) 19) (equal next-move 'right))))

(defun lose ()
  (with-ltk ()
	    (let* ((frame (make-instance 'frame :height 200 :width 300))
		   (text (make-instance 'label :master frame :text
					(random-elt '("Wow, that's really sad." "Well, that's just sad."))))
		   (button (make-instance 'button :master frame :text "Quit" :command #'exit-wish)))
	      (pack frame)
	      (pack text)
	      (pack button)))
  #'exit-wish)

(defun snake (&key (life-vals (zeros 400))(p2 nil))
  (with-ltk ()
	    (let* ((c (make-instance 'canvas :height 501 :width 501))
		   (grid-field (create-grid c life-vals))
		   (next-move 'right) (moved nil) (index 20) (s-length 3)
		   (next-move2 'left) (moved2 nil) (index2 379) (s-length2 -3)
		   (food-index (new-food-index life-vals))
		   (food (apply #'create-oval c (grid-coords food-index))))
	      (bind c "<KeyPress-q>"
		    (lambda (evt) (declare (ignore evt))
		      (exit-wish)))
	      (bind c "<KeyPress-Up>"
		    (lambda (evt) (declare (ignore evt))
		      (unless (or moved (equal next-move 'down))
			(setf next-move 'up) (setf moved t))))
	      (bind c "<KeyPress-Left>"
		    (lambda (evt) (declare (ignore evt))
		      (unless (or moved (equal next-move 'right))
			(setf next-move 'left) (setf moved t))))
	      (bind c "<KeyPress-Right>"
		    (lambda (evt) (declare (ignore evt))
		      (unless (or moved (equal next-move 'left))
			(setf next-move 'right) (setf moved t))))
	      (bind c "<KeyPress-Down>"
		    (lambda (evt) (declare (ignore evt))
		      (unless (or moved (equal next-move 'up))
			(setf next-move 'down) (setf moved t))))
	      (if p2 (progn
		(bind c "<KeyPress-w>"
		      (lambda (evt) (declare (ignore evt))
			(unless (or moved2 (equal next-move2 'down))
			  (setf next-move2 'up) (setf moved2 t))))
		(bind c "<KeyPress-s>"
		      (lambda (evt) (declare (ignore evt))
			(unless (or moved2 (equal next-move2 'up))
			  (setf next-move2 'down) (setf moved2 t))))
		(bind c "<KeyPress-a>"
		      (lambda (evt) (declare (ignore evt))
			(unless (or moved2 (equal next-move2 'right))
			  (setf next-move2 'left) (setf moved2 t))))
		(bind c "<KeyPress-d>"
		      (lambda (evt) (declare (ignore evt))
			(unless (or moved2 (equal next-move2 'left))
			  (setf next-move2 'right) (setf moved2 t))))))
	      (itemconfigure c food :fill 'red)
	      (pack c)
	      (force-focus c)
	      (loop while t
		    ;; Reset moved booleans, process keypress events
		    do (progn (setf moved nil) (setf moved2 nil) (process-events))
		    ;; Check boundaries
		    when (check-boundaries index next-move) return nil
		    when (and p2 (check-boundaries index2 next-move2)) return nil
		    ;; Move indices
		    do (incf index (index-move next-move))
		    when p2 do (incf index2 (index-move next-move2))
		    ;; Check for food collection
		    when (= index food-index)
		    do (progn (incf s-length) (setf food-index (move-food c food index life-vals)))
		    when (and p2 (= index2 food-index))
		    do (progn (decf s-length2) (setf food-index (move-food c food index2 life-vals)))
		    ;; Update life values if food hasn't moved
		    unless (or (= index food-index) (equal index2 food-index))
		    do (setf life-vals (update-life life-vals))
		    ;; Check for self-collisions
		    when (> (nth index life-vals) 0) return nil
		    when (and p2 (< (nth index2 life-vals) 0)) return nil
		    ;; Update life values at new indices
		    do (setf (nth index life-vals) s-length)
		    when p2 do (setf (nth index2 life-vals) s-length2)
		    ;; Finish up: draw and sleep
		    do (progn (draw-grid c grid-field life-vals) (sleep 1/8)))
	      (funcall (lose)))))

(snake)
