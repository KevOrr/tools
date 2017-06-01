(defun make-curriable (function arglist-length &optional args)
  (if (> arglist-length (length args))
      (lambda (&rest newargs)
        (make-curriable function arglist-length (append args newargs)))
      (apply function args)))

(defmacro lambda-curry (arglist &body body)
  `(make-curriable (lambda ,arglist
                     ,@body)
                   ,(length arglist)))

(defmacro defun-curry (name arglist &body body)
  `(defun ,name (&rest args)
     (apply (lambda-curry ,arglist
              ,@body)
            args)))

(defun-curry church-number (n f x)
  (if (> n 0)
      (funcall f (church-number (1- n) f x))
      x))

(defun-curry church-plus (m n f x)
  (church-number m f (church-number n f x)))

(defun-curry church-succ (n f x)
  (funcall f (church-number n f x)))

(defun-curry church-times (m n f x)
  (church-number m (church-number n f) x))

(defun-curry church-expt (m n f x)
  (funcall (church-number n (church-number m) f) x))

(defun-curry church-pred (n f x)
  (funcall (church-number n
                          (lambda-curry (g h)
                            (funcall h (funcall g f)))
                          (lambda-curry (u)
                            (declare (ignore u))
                            x))
           (lambda-curry (u) u)))

(defun-curry church-minus (m n f x)
  (funcall (church-number n #'church-pred) (church-number m f x)))
