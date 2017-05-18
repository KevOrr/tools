(defun floats-within-eps (a b epsilon)
  "Checks whether two numbers `a' and `b' are within epsilon of each other"
  (<= (- epsilon)
      (- a b)
      epsilon))

(defun find-combinations-that-sum-to (candidates sum &optional (epsilon single-float-epsilon))
  "Finds combinations of `candidates' whose sum is \"equal\" to `sum'.
   In actuality, their sum must be within epsilon of `sum'.
   I'm assuming both loop variables can fit within a fixnum, since testing this
   with `(= 36 (length candidates))' would take 2.3 hours in SBCL on an i5-6600k at 4.7 GHz"

  (let ((candidates (make-array (length candidates) :element-type 'number :initial-contents candidates)))
    (declare ((vector number) candidates))
    (loop :for selection fixnum :from 0 :below (ash 1 (length candidates))
          :if (floats-within-eps
               sum
               (loop :for i :of-type fixnum :from 0 :below (integer-length selection)
                     :if (logbitp i selection)
                       :sum (aref candidates i))
               epsilon)
            :collect (loop :for i :of-type fixnum :from 0 :below (integer-length selection)
                           :if (logbitp i selection)
                             :collect (aref candidates i)))))
