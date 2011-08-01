
(defmethod colstats ((col num) cell)
  (with-slots (min max) col
    (unless (col-ignorep col)
      (unless (no cell) 
	(when (> cell max) (format t "+") (setf max cell))
	(when (< cell min) (format t "-") (setf min cell)))))
  cell)

(defmethod colstats ((col sym) cell )
  (unless (no cell)
    (pushnew cell (sym-uniques col))
  cell))

(defmethod ranges! ((col sym))
  (declare (ignore b))
  (sym-uniqes col))

(defmethod ranges! ((col num))
  (loop for i upto (1- (num-bins col)) 
     collect i))

(defmethod discretize ((col sym) cell b)
  (declare (ignore b))
  cell)

(defmethod discretize ((col num) cell b)
  (with-slots (min max bins) col
    (setf bins b)
    (if (= cell max)
	b
	(floor
	 (/ (- cell min)
	    (/ (- max min) b))))))

(defmethod norm ((col sym) cell)
  cell)

(defmethod norm ((col num) cell)
  (with-slots (min max) col
    (if (no cell)
	cell
	(if (= max min)
	    0
	    (/ (- cell min) (- max min))))))

