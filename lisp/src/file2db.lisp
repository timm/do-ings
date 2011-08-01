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

(defun xindex (cols rows)
  (let ((h (make-hash-table :test 'equal)))
    (doitems (row r rows h)
	     (mapc (lambda (col cell)
		     (push r 
			   (gethash `(,(col-pos col) ,cell)  h)))
		   cols row))))
    
(defun mapcells (fn cols rows)
  "fn is called on the i-th col and cell in all rows"
    (mapcar (lambda(row)
	      (mapcar fn cols row))
	    rows))

(defun 5bins (col cell)
  (discretize col cell 5))

(defun file2db(f &optional 
	       (statistics 'colstats) 
	       (discretizer '5bins))
  (let* ((all   (file2list f))
	 (rows  (cdr all))
	 (head  (car all))
	 (name  (pop head))
	 (cols  (cols head)))
    (mapcells statistics cols rows)
    (setf rows (mapcells discretizer cols rows))
    (make-db
     :name name
     :rows rows
     :cols cols
     :xindex (xindex cols rows))))

