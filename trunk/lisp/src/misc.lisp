
(Defun q () (quit))

(defun 2d-print (a)
  (terpri)
  (loop for i below (car (array-dimensions a)) do
       (loop for j below (cadr (array-dimensions a)) do
            (format t "~a" (aref a i j)))
       (format t "~%")))

(defun noop (&rest args)
  (declare (ignore args)))

(defun vector! (v)
  (if (vectorp v) 
      v
      (coerce v 'vector)))