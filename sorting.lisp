(defun array-copy (data)
  (make-array (array-dimensions data) :initial-contents data))

(defun shuffle (data &optional (random-func #'random))
  (declare (vector data))
  (loop :with length := (array-dimension data 0)
        :with data := (array-copy data)
        :for i :from 0 :below (1- length)
        :do (rotatef (aref data i) (aref data (+ i (funcall random-func (- length i)))))
        :finally (return data)))

(defun selection-sort (data)
  (declare (vector data))
  (loop :with length := (array-dimension data 0)
        :with data := (array-copy data)
        :for target :from 0 :below (1- length) :do
          (let ((next-minimum
                  (loop :with minimum-position := target
                        :for candidate :from (1+ target) :below length
                        :if (< (aref data candidate) (aref data minimum-position))
                          :do (setf minimum-position candidate)
                        :finally (return minimum-position))))
            (rotatef (aref data target)
                     (aref data next-minimum)))
        :finally (return data)))

(defun insertion-sort (data)
  (declare (vector data))
  (loop :with length := (array-dimension data 0)
        :with data := (array-copy data)
        :for next :from 1 :below length :do
          (loop :with next := next
                :while (and (plusp next)
                            (> (aref data (1- next)) (aref data next)))
                :do (rotatef (aref data (1- next)) (aref data next))
                    (decf next))
        :finally (return data)))
