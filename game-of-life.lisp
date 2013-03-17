(ql:quickload 'cl-charms)

(defpackage #:game-of-life
  (:use #:cl #:cl-charms)
  (:nicknames #:gol))

(in-package #:gol)

(defconstant +xmax+ 80)
(defconstant +ymax+ 80)

(defvar *board* (make-array (list +xmax+ +ymax+) :initial-element nil))

(defun update-board ()
  (let ((updates nil))
    (dotimes (x +xmax+)
            (dotimes (y +ymax+)
              (push (get-cell-update x y) updates)))
    (perform-updates updates)))

(defun perform-updates (updates)
  (dolist (update updates)
    (when update
      (let ((fn (first update))
            (args (second update)))
        (apply fn args)))))

(defun get-cell-update (x y)
  (let ((nn (num-neighbors x y))
        (alive (not (null (aref *board* x y)))))
    (cond
      ((and alive (or (> nn 3) (< nn 2))) (list #'kill (list x y)))
      ((and (not alive) (= nn 3)) (list #'birth (list  x y)))
      (t nil))))

(defun kill (x y)
  (setf (aref *board* x y) nil))

(defun birth (x y)
  (setf (aref *board* x y) 't))

(defun num-neighbors (x y)
  (let ((dirs '((-1 0) (-1 -1) (-1 1) (1 -1) (1 0) (1 1) (0 1) (0 -1))))
    (flet ((neighbor (dir)
             (neighbor x y dir)))
      (reduce #'+ (mapcar #'neighbor dirs)))))

(defun neighbor (x y dir)
  (let ((nx (+ x (first dir)))
        (ny (+ y (second dir))))
    (if (or (minusp nx) (>= nx +xmax+)
            (minusp ny) (>= ny +ymax+)
            (null (aref *board* nx ny)))
        0
        1)))

(defun run ()
  (init)
  (game-loop)
  (exit-life))

(defun init ()
  (initscr)
  (raw)
  (keypad *stdscr* TRUE)
  (curs-set 0)
  (noecho))

(defun game-loop ()
  (dotimes (i 20)
    (progn (draw-board)
           (update-board)
           (sleep 0.5))))

(defun draw-board ()
    (dotimes (x +xmax+)
      (dotimes (y +ymax+)
        (if (aref *board* x y)
            (draw x y (char-code #\*))
            (draw x y (char-code #\Space)))))
    (refresh))

(defun draw (x y ch)
  (mvaddch x y ch))

(defun exit-life ()
  (endwin)
  (sb-ext:exit))

(defun populate (coords)
  (loop for (x y) on coords by #'cddr
       do (setf (aref *board* x y) 't)))

(populate '(5 5
            5 6
            5 7))
(run)
