(defun query (q d)
  (query-and
   (mapcar (lambda (q1)
	     (query-or q1 (db-xindex d)))
	   (group-query q))))

(defun group-query (q)
  (let ((out 
	 (mapcar 'list (remove-duplicates 
			(mapcar #'car q)))))
    (dolist (one q out)
      (let ((a (first one))
	    (v (second one)))
	(pushnew v (cdr (assoc a out)))))))

(defun query-or (q1 xindex)
  (let ((a  (car q1))
	out)
    (dolist (v (cdr q1) out)
      (setf out
	    (union out 
		   (gethash `(,a ,v) xindex d))))))

(defun query-and (results)
  (setf results (sort results 
		      (lambda (a b)
			(< (length a) (length b)))))
  (unless (null results)
    (let ((out (car results)))
      (dolist (result (cdr results) out)
	(if (null (setf out (interesection out results)))
	    (return-from query-and nil))))))

    