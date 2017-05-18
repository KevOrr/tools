(eval-when (:compile-toplevel)
  (ql:quickload :generators))

(in-package :generators)

(defun pop-hash (key hash-table &optional default)
  (prog1 (gethash key hash-table default)
    (remhash key hash-table)))

;; https://code.activestate.com/recipes/117119-sieve-of-eratosthenes/#c4
(defun prime-generator ()
  (make-generator ()
    (yield 2)
    (let ((D (make-hash-table)))
      (loop :for q := 3 :then (+ q 2)
            :for p := (pop-hash q D)
            :do (cond (p (loop :for x := (+ q p p) :then (+ x p p)
                               :while (gethash x D)
                               :finally (setf (gethash x D) p)))
                      (t (yield q)
                         (setf (gethash (* q q) D) q)))))))

;; Same as above function, just collect into list instead of yielding
(defun prime-list (limit)
  (declare ((integer 1) limit)
           (optimize speed (safety 0) (space 0)))
  (let ((output (make-array limit :element-type '(integer 0) :initial-element 0))
        (D (make-hash-table)))
    (setf (aref output 0) 2)
    (loop :for q := 3 :then (+ q 2)
          :for p := (pop-hash q D)
          :with i := 1
          :while (eql 0 (aref output (1- limit)))
          :do (cond (p (loop :for x := (+ q p) :then (+ x p)
                             :while (gethash x D)
                             :finally (setf (gethash x D) p)))
                    (t (setf (aref output i) q)
                       (incf i)
                       (setf (gethash (* q q) D) (ash q 1)))))
    output))

;; https://code.activestate.com/recipes/117119-sieve-of-eratosthenes/#c17
;; limit here is the non-inclusive upper-bound for the largest prime generated
(defun prime-generator-finite (limit)
  (declare ((integer 3) limit))
  (make-generator ()
    (yield 2)
    (let ((f (make-array limit :element-type 'bit :initial-element 1)))
      (loop :for p :from 3 :to (sqrt limit) :by 2
            :if (eql 1 (aref f p))
              :do (yield p)
                  (loop :for q :from (* p p) :to limit :by (ash p 1)
                        :do (setf (aref F q) 0)))
      (loop :for p :from (+ 2 (floor (sqrt limit))) :below limit :by 2
            :if (eql 1 (aref f p))
              :do (yield p)))))

(defun recursive-prime-generator ()
  (flet ((ifilter (pred gen)
           (make-generator ()
             (iter:iter
               (for i :in-generator gen)
               (if (funcall pred i) (yield i)))))
         (it-count (start &optional (step 1))
           (make-generator ()
             (loop :for i :from start :by step
                   :do (yield i)))))
    (break)
    (let ((seq (it-count 2)))
      (loop :for p := (next seq)
            :do (setq seq (ifilter (lambda (i)
                                     (eql 0 (mod p i)))
                                   seq))
                (print p)))))
