(ql:quickload :ltk)
(use-package :ltk)

(defun _/ (x y) (floor (/ x y)))

(defun draw-board (canvas x y height width &key (margin 20))
  (let ((x1 (+ x (_/ width 3))) (x2 (+ x (_/ width 3/2)))
	(xm1 (+ x margin)) (xm2 (+ x width (- margin)))
	(y1 (+ y (_/ height 3))) (y2 (+ y (_/ height 3/2)))
	(ym1 (+ y margin)) (ym2 (+ y height (- margin))))
    (create-line canvas (list x1 ym1 x1 ym2))
    (create-line canvas (list x2 ym1 x2 ym2))
    (create-line canvas (list xm1 y1 xm2 y1))
    (create-line canvas (list xm1 y2 xm2 y2))))

(defun draw-x (canvas coords &key (width 55)(height 55)(margin 10))
  (let ((x (car coords))(y (cadr coords)))
    (itemconfigure canvas
		   (create-line canvas (list (+ x margin) (+ y margin)
					     (+ x width (- margin)) (+ y height (- margin))))
		   :fill 'blue)
    (itemconfigure canvas
		   (create-line canvas (list (+ x margin) (+ y height (- margin))
					     (+ x width (- margin)) (+ y margin)))
		   :fill 'blue)))

(defun draw-o (canvas coords &key (width 55)(height 55)(margin 10))
  (let ((x (car coords)) (y (cadr coords)))
    (itemconfigure canvas
		   (create-oval canvas (+ x margin) (+ y margin)
				       (+ x width (- margin)) (+ y height (- margin)))
		   :outline 'red)))

(defun event->nw-tile-coords (evt)
  (mapcar (lambda (n) (loop for i from 0 to 500 by 55 when (> i n) return (- i 55)))
	  `(,(event-x evt) ,(event-y evt))))

(defun symbol->draw-fun (sym)
  (cond ((equal sym 'x) #'draw-x)
	((equal sym 'o) #'draw-o)
	(t nil)))

(defun event->game-coords (evt)
  (let* ((coords (event->nw-tile-coords evt))
	 (screen-x (car coords))(screen-y (cadr coords))
	 (small-x (_/ screen-x 55))(small-y (_/ screen-y 55))
	 (sub-x (mod small-x 3))(sub-y (mod small-y 3))
	 (big-x (_/ small-x 3))(big-y (_/ small-y 3)))
    `(,(+ big-x (* 3 big-y)) ,(+ sub-x (* 3 sub-y)))))

(defun try-move (sym canvas evt boards)
  (let* ((game-c (event->game-coords evt))
	(big-c (car game-c))(small-c (cadr game-c)))
    (if (null (nth small-c (nth big-c boards)))
      (progn (setf (nth small-c (nth big-c boards)) sym)
	     (funcall (symbol->draw-fun sym) canvas (event->nw-tile-coords evt)) boards)
      nil)))

(defun check-victory (board)
  (destructuring-bind (a b c d e f g h i) board
    (reduce (lambda (x y) (or x y))
	    (mapcar (lambda (l)
		      (reduce (lambda (x y) (if (and x y (symbolp x) (symbolp y) (equal x y)) x nil)) l))
		    `((,a ,b ,c) (,d ,e ,f) (,g ,h ,i)
		      (,a ,d ,g) (,b ,e ,h) (,c ,f ,i)
		      (,a ,e ,i) (,c ,e ,g) )))))

(defun check-grand-victory (board)
  (check-victory (mapcar #'check-victory board)))

(defun restrict-cell (arg) (if (null arg) -1 arg))

(defmacro update (canvas board winner)
  `(progn (setf ,board
	 (loop with tmp-board = ,board
	       for i from 0 to 8
	       for ix = (* (mod i 3) 166)
	       for iy = (* (_/ i 3) 166)
	       for sieg = (check-victory (nth i ,board))
	       when sieg do (progn (funcall (symbol->draw-fun sieg) ,canvas `(,ix ,iy) :width 166 :height 166)
				   (setf (nth i tmp-board) (mapcar #'restrict-cell (nth i tmp-board))))
	       finally (return tmp-board)))
	  (setf ,winner (check-grand-victory ,board))))

(defun game ()
  (with-ltk ()
	    (let* ((field (make-instance 'canvas :height 500 :width 500))
		   (boards (loop repeat 9 collect (loop repeat 9 collect nil)))
		   (winner nil))
	      (bind field "<ButtonPress-1>" (lambda (evt) (unless winner (progn (try-move 'x field evt boards)
					      (update field boards winner)))))
	      (bind field "<ButtonPress-3>" (lambda (evt) (unless winner (progn (try-move 'o field evt boards)
					      (update field boards winner)))))
	      (bind field "<KeyPress-q>" (lambda (evt) (declare (ignore evt)) (exit-wish)))
	      (pack field)
	      (itemconfigure field (create-rectangle field 0 0 499 499) :fill 'white)
	      (force-focus field)
	      (draw-board field 1 1 500 500)
	      (loop for ix from 0 to 2
		    do (loop for iy from 0 to 2 do
			     (draw-board field (* ix 166) (* iy 166) 166 166))))))

(game)
