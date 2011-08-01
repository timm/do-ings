(defun make-utils (l)
  (when l
    (let ((one (pop l))
	  (two (pop l)))
      (cons `(,one . ,two)
	    (make-utils l)))))

      

(defun tar3 (f &optional (b 3) &rest utils)
  (let ((d (file2db f)))
    (setf (db-fx d) (make-utils utils))
    (dohash (key value (db-xindex d))
      (print `(key ,key value ,value)))))
