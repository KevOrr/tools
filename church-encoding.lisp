(defpackage curry
  (:use cl)
  (:export defun-curry lambda-curry make-curriable))

(in-package curry)

(defun make-curriable (function arglist-length &optional args)
  (cond ((< (length args) arglist-length)
         (lambda (&rest newargs)
           (make-curriable function arglist-length (append args newargs))))

        ((> (length args) arglist-length) ; function must return another curriable function
         (make-curriable (apply function (subseq args 0 arglist-length))
                         1
                         (append (subseq args arglist-length))))

        (t (apply function args))))

(defmacro lambda-curry (arglist &body body)
  `(make-curriable (lambda ,arglist
                     ,@body)
                   ,(length arglist)))

(defmacro defun-curry (name arglist &body body)
  `(defun ,name (&rest args)
     (apply (lambda-curry ,arglist
              ,@body)
            args)))



(defpackage church
  (:use cl curry)
  (:shadow + 1+ * 1- - expt
           not and or
           zerop <= >= = < >
           first second
           nil null cons car cdr)
  (:export church + 1+ * expt 1- -
           true false church-bool unchurch-bool not and or xor
           zerop <= >= = < >
           pair first second
           nil null cons car cdr
           read-church-numeral enable-church-numeral-read-macro))

(in-package church)

;; Church Numerals

;; TODO investigate funcallable-standard-object, in order to print church numerals
(defun-curry church (n f x)
  (declare (integer n))
  (if (cl:> n 0)
      (funcall f (church (cl:1- n) f x))
      x))

(defun-curry + (m n f x)
  (funcall m f (funcall n f x)))

(defun-curry 1+ (n f x)
  (funcall f (funcall n f x)))

(defun-curry * (m n f)
  (funcall m (funcall n f)))

(defun-curry expt (m n)
  (funcall n (funcall m)))

(defun-curry 1- (n f x)
  (funcall n
           (lambda-curry (g h)
             (funcall h (funcall g f)))
           (lambda-curry (u)
             (declare (ignore u))
             x)
           (lambda-curry (u) u)))

(defun-curry - (m n)
  (funcall n #'1- (funcall m)))


;; Church Booleans

(defun-curry true (p q)
  (declare (ignore q))
  p)

(defun-curry false (p q)
  (declare (ignore p))
  q)

(defun church-bool (bool)
  (if bool #'true #'false))

(defun unchurch-bool (churched-bool)
  (declare ((member #'true #'false) churched-bool))
  (if (eq churched-bool #'true) t cl:nil))

(defun-curry not (p) (funcall p #'false #'true))
(defun-curry and (p q) (funcall p q p))
(defun-curry or (p q) (funcall p p q))
(defun-curry xor (p q) (funcall p (not q) q))


;; Predicates

(defun-curry zerop (n)
  (funcall n (lambda-curry (x)
               (declare (ignore x))
               #'false)
           #'true))

(defun-curry <= (m n)
  (zerop (- m n)))

(defun-curry >= (m n)
  (<= n m))

(defun-curry = (m n)
  (and (<= m n) (<= n m)))

(defun-curry < (m n)
  (and (<= m n) (not (>= m n))))

(defun-curry > (m n)
  (< n m))


;; Pairs

(defun-curry pair (x y z)
  (funcall z x y))

(defun-curry first (p)
  (funcall p (lambda-curry (x y)
               (declare (ignore y))
               x)))

(defun-curry second (p)
  (funcall p (lambda-curry (x y)
               (declare (ignore x))
               y)))


;; Lists

(defun-curry nil () (pair #'true #'true))
(defun-curry null () #'first)
(defun-curry cons (head tail) (pair #'false (pair head tail)))
(defun-curry car (z) (first (second z)))
(defun-curry cdr (z) (second (second z)))


;; Division

;; Recursive definition
(defun-curry divide (n m f x)
  (break)
  (funcall
   (lambda-curry (diff)
     (>= diff 0
         (funcall f (divide diff m f x))
         (church 0 f x)))
   (- n m)))



;; Reader macro for church numerals
(defun read-church-numeral (stream macro-char &optional arg)
  (declare (ignore macro-char arg))
  (let ((n (read stream)))
    (if (integerp n)
        (church n)
        `(church ,n))))

(defun enable-church-numeral-read-macro (&optional (char1 #\#) (char2 #\N))
  (if char2
      (set-dispatch-macro-character char1 char2 #'read-church-numeral)
      (set-macro-character char1 #'read-church-numeral)))
