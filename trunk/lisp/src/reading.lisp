(defun eat-line (str n  fn &optional (line (read str nil)))
  (cond ((null  line)  n)
	(t 
	 (funcall fn line (incf n))
	 (eat-line str n fn))))

(defun eat-lines (f &optional (fn 'noop) &aux (n 0))
  (with-open-file (str f)
    (read str nil)
    (eat-line str n fn )))

(defun eat-header (f &optional (fn 'noop))
  (with-open-file (str f) 
    (let ((line (read str nil)))
      (if line 
	  (funcall fn line)))))

(defun reads (f)
  (if (symbolp f)
      (setf f (string-downcase
	       (format nil "~a.lisp" f))))
  (with-open-file (str f)
    (reads1 str)))

(defun reads1 (str &aux (line (read str nil)))
  (when line
    (cons line (reads1 str))))
