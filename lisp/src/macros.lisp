(defmacro doitems ((one n list &optional out) &body body )
  `(let ((,n -1))
     (dolist (,one ,list ,out) (incf ,n) ,@body)))


(defmacro while (test &rest body)
  `(loop (unless ,test (return nil))
         ,@body))

(defmacro dohash ((key value hash &optional out) &body body)
  `(progn 
     (maphash (lambda (,key ,value)
		,@body) ,hash)
     ,out))

(defmacro dov ((one v &optional out) &body body )
  `(progn (loop for ,one across ,v do ,@body)
	  ,out))

(defmacro dov2 ((n one two v1 v2 &optional out) &body body )
  (let ((max (gensym)))
    `(let ((,max (length ,v1)))
       (dotimes (,n ,max ,out)
	 (let ((,one (svref ,v1 ,n))
	       (,two (svref ,v2 ,n)))
	   ,@body)))))

