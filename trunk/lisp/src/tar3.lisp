(declaim (inline thingp no))
(defun thingp (x y) (and (symbolp x) (find y (symbol-name x))))
(defun goalp    (x) (thingp x #\!))
(defun nump     (x) (thingp x #\$))
(defun no       (x) (thingp x #\?))

(defun cols(l)
  (let ((n 0))
    (mapcar (lambda (word) 
	      (funcall (if (nump word) 'make-num 'make-sym)
		       :name word :pos (incf n) 
		       :goalp (goalp word) :ignorep (no word)))
	    l)))

(defmethod colstats ((col num) x)
  (with-slots (min max) col
    (unless (col-ignorep col)
      (unless (no x) 
	(when (> x max) (format t "+") (setf max x))
	(when (< x min) (format t "-") (setf min x))))))

(defmethod colstats ((col sym) x)
  (pushnew x (col-range col)))

;;;; reader
(defun norm (col x)
  (with-slots (min max) col
    (if (no x)
	x
	(if (= max min)
	    0
	    (/ (- x min) (- max min))))))

(defun file2db(f)
  (let* ((all   (reads f))
	 (head  (car all))
	 (name  (pop head))
	 (cols  (cols head)))
    (make-db
     :name name
     :rows (vector!
	    (mapcar (lambda (row)
		      (mapc 'colstats cols row)
		      (vector! row))
		    (cdr all)))
     :cols (vector! cols))))

(defun make-utils (l)
  (when l
    (cons (cons (pop l) (pop l))
	  (make-utils l))))

(defun descretize (d b)
  (dov (col (db-cols d))
       (setf (col-range b col) (ranges! col)))
  (dov (row (db-rows d))
       (dov2 (n col cell (db-cols cols) row)
	     (setf (svref row n)
		   (discretize1 col cell b)))))

(defmethod ranges! ((col sym) b)
  (declare (ignore b))
  (col-range col))

(defmethod ranges! ((col num) b)
  (loop for i in 1 to (1- b) collect i))

(defmethod descretize1 ((col sym) cell b)
  (declare (ignore b col cell)))

(defmethod discretize1 ((col num) cell b)
  (with-slots (min max range) col
    (setf range b)
    (floor
     (/ (- cell min)
	(/ (- max min) b)))))
  
(defun round0 (d)
  (let (out)
    (dov (col (db-cols d))
	 (dolist (range (col-ranges col))
	   (push (cons (score col )))))))

(defun tar3 (f &key (b 3) &rest utils)
  (let ((d (file2db f)))
    (setf (db-fx d) (make-utils utils))
    (descretize d b)
    (let ((ranges (round0 d))))))
	   
