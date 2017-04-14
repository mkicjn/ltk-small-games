(ql:quickload 'ltk)
(use-package :ltk)

(setf *random-state* (make-random-state t))

(defun int-coords (index &optional (maxx 20))
  "Returns 2-D coordinates given a linear coordinate"
  (list (mod index maxx) (floor (/ index maxx))))

(defun grid-coords (index &optional (maxx 20) &aux (coords (int-coords index maxx)))
  "Returns screen coordinates given a linear coordinate"
  (mapcar #'1+ (list (* (car coords) 25) (* (cadr coords) 25)
		     (* (1+ (car coords)) 25) (* (1+ (cadr coords)) 25))))

(defun update-life (life-vals)
  "Pushes every item in list towards zero by one"
  (mapcar (lambda (x) (if (zerop x) 0 (funcall (if (> x 0) #'1- #'1+) x))) life-vals))

(defun create-grid (canvas life-vals)
  "Draws a new grid of rectangles on the screen"
  (loop for life in life-vals for i from 0
	collect (apply #'create-rectangle canvas (grid-coords i))))

(defun draw-grid (canvas grid life-vals)
  "Recolors the grid to reflect life values in each cell"
  (loop for life in life-vals for i from 0
	do (itemconfigure canvas (nth i grid) :fill (if (zerop life) 'white
						      (if (> life 0) 'green 'blue)))))

(defun index-move (direction)
  "Returns offset given direction symbol"
  (ccase direction (up -20) (left -1) (right 1) (down 20)))

(defun random-elt (l)
  "Chooses a random element in a list"
  (nth (random (length l)) l))

(defun new-food-index (life-vals)
  "Returns a linear coordinate for the food's new position"
  (let ((p (loop for life in life-vals for i from 0 if (zerop life) collect i))) (random-elt p)))

(defun move-food (canvas food old-index life-vals &aux (new-index (new-food-index life-vals)))
  "Physically relocates the food circle"
  (let ((deltas (mapcar #'- (grid-coords new-index) (grid-coords old-index))))
  (itemmove canvas food (car deltas) (cadr deltas))) new-index)

(defun check-boundaries (index next-move)
  "Checks whether a snake is about to go off the screen"
  (or (and (< index 20) (equal next-move 'up))
      (and (> index 379) (equal next-move 'down))
      (and (= (mod index 20) 0) (equal next-move 'left))
      (and (= (mod index 20) 19) (equal next-move 'right))))

(defun lose ()
  "Creates a window with a losing message and quit button"
  (with-ltk ()
	    (let* ((frame (make-instance 'frame :height 200 :width 300))
		   (text (make-instance 'label :master frame :text
					(random-elt '("Wow, that's really sad." "Well, that's just sad."))))
		   (button (make-instance 'button :master frame :text "Quit" :command #'exit-wish)))
	      (pack frame)
	      (pack text :pady 20 :padx 10)
	      (pack button :pady 20 :padx 10)))
  #'exit-wish)

(defun snake (&key (p2 nil)(speed 8))
  "Creates a new snake game window. Keyword p2 indicates a second player"
  (with-ltk ()
	    (let* ((c (make-instance 'canvas :height 501 :width 501))
		   (life-vals (loop repeat 400 collect 0))
		   (grid-field (create-grid c life-vals))
		   (next-move 'right) (moved nil) (index 20) (s-length 3)
		   (next-move2 'left) (moved2 nil) (index2 379) (s-length2 -3)
		   (food-index (new-food-index life-vals))
		   (food (apply #'create-oval c (grid-coords food-index))))
	      (bind c "<KeyPress-space>"
		    (lambda (evt) (declare (ignore evt))
		      (setf speed (* 2 speed))))
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
	      (loop
		    ;; Reset moved booleans, process keypress events
		    do (progn (setf moved nil) (setf moved2 nil) (process-events))
		    ;; Check boundaries
		    when (check-boundaries index next-move) return nil
		    when (and p2 (check-boundaries index2 next-move2)) return nil
		    ;; Move indices
		    do (incf index (index-move next-move))
		    when p2 do (incf index2 (index-move next-move2))
		    ;; Update life values if food isn't collected
		    unless (or (= index food-index) (equal index2 food-index))
		    do (setf life-vals (update-life life-vals))
		    ;; Update snake length and food location if collected
		    when (= index food-index)
		    do (progn (incf s-length) (setf food-index (move-food c food index life-vals)))
		    when (and p2 (= index2 food-index))
		    do (progn (decf s-length2) (setf food-index (move-food c food index2 life-vals)))
		    ;; Check for self-collisions
		    when (not (zerop (nth index life-vals))) return nil
		    when (and p2 (not (zerop (nth index2 life-vals)))) return nil
		    ;; Update life values at new indices
		    do (setf (nth index life-vals) s-length)
		    when p2 do (setf (nth index2 life-vals) s-length2)
		    ;; Finish up: draw and sleep
		    do (progn (draw-grid c grid-field life-vals) (sleep (/ 1 speed))))
	      (funcall (lose)))))

(defun menu ()
  "Creates a menu with buttons for choosing a snake game mode"
  (with-ltk ()
	    (wm-title *tk* "Snake")
	    (let* ((button1 (make-instance 'button :text "One player"
					   :command #'snake))
		   (button2 (make-instance 'button :text "Two-player"
					   :command (lambda () (snake :p2 t)))))
	      (pack button1 :padx 10 :pady 10)
	      (pack button2 :padx 10 :pady 10))))

(menu)
