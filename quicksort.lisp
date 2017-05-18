(defun slice (vector a b &optional (type t) (dimension (- b a)))
  (make-array dimension :element-type type :displaced-to vector :displaced-index-offset a))

(defun inplace-quicksort (vector &optional (comparison #'<))
  (declare (vector vector))
  (if (<= (length vector) 1)
      vector
      (loop :with pivot := 0
            :with compare-to := (1- (length vector))
            :until (= pivot compare-to)
            :if (< pivot compare-to)
              :do (cond ((not (funcall comparison (aref vector pivot) (aref vector compare-to)))
                         (rotatef (aref vector pivot) (aref vector compare-to))
                         (rotatef pivot compare-to)
                         (incf compare-to))
                        (t
                         (decf compare-to)))
            :if (> pivot compare-to)
              :do (cond ((funcall comparison (aref vector pivot) (aref vector compare-to))
                         (rotatef (aref vector pivot) (aref vector compare-to))
                         (rotatef pivot compare-to)
                         (decf compare-to))
                        (t
                         (incf compare-to)))
            :finally
               (inplace-quicksort (slice vector 0 pivot) comparison)
               (inplace-quicksort (slice vector (1+ pivot) (length vector)) comparison))))

(defun quicksort (vector &optional (comparison #'<))
  (declare (vector vector))
  (let ((copied (make-array (length vector) :initial-contents vector)))
    (inplace-quicksort copied comparison)
    copied))
