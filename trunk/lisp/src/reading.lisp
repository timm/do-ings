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

(defun file2list (f &optional (ext ".lisp"))
  (labels ((worker (stream &aux (line (read stream nil)))
	     (when line (cons line (worker stream))))
	   (filename (f)
	     (if (stringp f) f (format nil "~(~a~a~)" f ext))))
    (with-open-file (stream (filename f))
      (worker stream))))
