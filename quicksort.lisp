(defun swap-elements (vector a b)
  (declare (vector vector))
  (let ((temp (aref vector a)))
    (setf (aref vector a) (aref vector b))
    (setf (aref vector b) temp)))

(defmacro swap-forms (a b)
  `(let ((temp ,a))
     (setf ,a ,b)
     (setf ,b temp)))

(defun quicksort (vector &optional (comparison #'<))
  (declare (vector vector))
  (if (<= (length vector) 1)
      vector
      (loop :with pivot := 0
            :with compare-to := (1- (length vector))
            :until (= pivot compare-to)
            :if (< pivot compare-to)
              :do (cond ((not (funcall comparison (aref vector pivot) (aref vector compare-to)))
                         (swap-elements vector pivot compare-to)
                         (swap-forms pivot compare-to)
                         (incf compare-to))
                        (t
                         (decf compare-to)))
            :if (> pivot compare-to)
              :do (cond ((funcall comparison (aref vector pivot) (aref vector compare-to))
                         (swap-elements vector pivot compare-to)
                         (swap-forms pivot compare-to)
                         (decf compare-to))
                        (t
                         (incf compare-to)))
            :finally
              (quicksort (make-array pivot :displaced-to vector :displaced-index-offset 0) comparison)
              (quicksort (make-array (- (length vector) pivot 1)
                                     :displaced-to vector :displaced-index-offset (1+ pivot))
                         comparison))))
