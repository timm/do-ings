
(defstruct col 
	   name  pos  goalp 
	   ignorep (weight 1))

(defstruct (sym (:include col)) uniques)
(defstruct (num (:include col))
  (bins 5)
  (min most-positive-fixnum) 
  (max most-negative-fixnum))

(defstruct db name cols rows fx  
	   (xindex (make-hash-table :test 'equal)))

