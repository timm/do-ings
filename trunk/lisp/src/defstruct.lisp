(defstruct col name range pos goalp ignorep (weight 1))

(defstruct (sym (:include col)))
(defstruct (num (:include col))
  (min most-positive-fixnum) 
  (max most-negative-fixnum))

(defstruct db name cols rows fx)