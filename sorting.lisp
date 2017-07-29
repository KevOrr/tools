(defun array-copy (data)
  (make-array (array-dimensions data) :initial-contents data))

(defun shuffle (data &optional (random-func #'random))
  (declare (vector data))
  (let ((data (array-copy data)))
    (loop :with length := (length data)
          :for i :from 0 :below (1- length)
          :do (rotatef (aref data i) (aref data (+ i (funcall random-func (- length i))))))
    data))

(defun selection-sort (data)
  (declare (vector data))
  (let ((data (array-copy data)))
    (loop :with length := (length data)
          :for target :from 0 :below (1- length) :do
            (let ((next-minimum
                    (loop :with minimum-position := target
                          :for candidate :from (1+ target) :below length
                          :if (< (aref data candidate) (aref data minimum-position))
                            :do (setf minimum-position candidate)
                          :finally (return minimum-position))))
              (rotatef (aref data target)
                       (aref data next-minimum))))
    data))

(defun insertion-sort (data)
  (declare (vector data))
  (let ((data (array-copy data)))
    (loop :with length := (length data)
          :for next :from 1 :below length :do
            (loop :with next := next
                  :while (and (plusp next)
                              (> (aref data (1- next)) (aref data next)))
                  :do (rotatef (aref data (1- next)) (aref data next))
                      (decf next)))
    data))

(defun merge-sort (data)
  (declare (vector data))
  (labels ((helper (data lower upper)
             (let ((middle (floor (+ lower upper) 2))
                   (length (- upper lower)))
               (when (> length 1)
                 (helper data lower middle)
                 (helper data middle upper)
                 (loop :with temp := (make-array length)
                       :with temp-index := 0
                       :with lower-index := lower
                       :with upper-index := middle
                       :do (cond ((= lower-index middle)
                                  (setf (subseq temp temp-index)
                                        (subseq data upper-index upper))
                                  (replace data temp :start1 lower)
                                  (return))
                                 ((= upper-index upper)
                                  (setf (subseq temp temp-index)
                                        (subseq data lower-index middle))
                                  (replace data temp :start1 lower)
                                  (return))
                                 ((> (aref data lower-index) (aref data upper-index))
                                  (setf (aref temp temp-index) (aref data upper-index))
                                  (incf temp-index)
                                  (incf upper-index))
                                 (t
                                  (setf (aref temp temp-index) (aref data lower-index))
                                  (incf temp-index)
                                  (incf lower-index))))))
             data))
    (helper (array-copy data) 0 (length data))))

(defun radix-sort (data elt-func count-func radix)
  (labels ((helper (data elt-func radix current-digit size)
             (let ((buckets (loop :repeat radix :collect nil)))
               (case (length data)
                 ((0 1) data)
                 (t
                  (loop :for i :in data
                        :do (let ((digit (funcall elt-func i current-digit)))
                              (setf (elt buckets digit) (cons i (elt buckets digit)))))
                  (if (< current-digit (1- size))
                      (loop :for bucket :in buckets
                            :nconc (helper (nreverse bucket) elt-func radix (1+ current-digit) size))
                      (nconc (mapcar #'nreverse buckets))))))))
    (let ((data (coerce data 'list))
          (length-table (make-hash-table)))
      (loop :for i :in data
            :for length := (funcall count-func i)
            :do (cond ((gethash length length-table)
                       (setf (gethash length length-table)
                             (cons i (gethash length length-table))))
                      (t
                       (setf (gethash length length-table)
                             (list i)))))
      (let ((sorted-lengths
              ;; cheating?
              (sort (loop :for length :being :each :hash-key :in length-table :using (:hash-value list)
                          :collect (list length list))
                    #'> :key #'first)))

        (loop :for length-list :in sorted-lengths
              :nconc (helper (nreverse (second length-list)) elt-func radix 0 (first length-list)))))))

(defun slowsort (data)
  (labels ((inplace-slowsort (data &optional (a 0) (b (max 0 (1- (length data)))))
             (declare (vector data)
                      (fixnum a b))
             (when (< a b)
               (let ((m (floor (+ a b) 2)))
                 (inplace-slowsort data a m)
                 (inplace-slowsort data (1+ m) b)
                 (if (< (aref data b) (aref data m))
                     (rotatef (aref data b) (aref data m)))
                 (inplace-slowsort data a (1- b))))))
    (let ((data (array-copy data)))
      (inplace-slowsort data)
      data)))

(defun test-sorts (unsorted &optional (test-funcs '(selection-sort insertion-sort)))
  (let ((sorted (sort (array-copy unsorted) #'<)))
    (loop :for func :in test-funcs
          :unless (equalp (funcall func unsorted) sorted)
            :do (error "~W returned an unsorted vector" func))))

(defun time-sort (function lower upper &optional (divisions 10))
  (flet ((time-exec (function &rest args)
           (let ((begin (get-internal-run-time)))
             (apply function args)
             (float (/ (- (get-internal-run-time) begin) internal-time-units-per-second)))))
    (loop :for i :from lower :to upper :by (floor (- upper lower) divisions)
          :do (print i)
          :collect (list i (time-exec function (coerce (loop :for j :from 1 :to i
                                                             :collect j)
                                                       'vector))))))
