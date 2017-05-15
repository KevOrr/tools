;; https://www.reddit.com/r/redditdata/comments/6640ru/place_datasets_april_fools_2017/

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-gd))

(in-package :cl-gd)

(defstruct csv-record
  ;; Author: Elijah Malaby
  (timestamp 0 :type (integer 0))
  (user-id "" :type string)
  (x 0 :type integer)
  (y 0 :type integer)
  (color 0 :type integer))

(defun parse-csv-line (stream)
  ;; Author: Elijah Malaby
  (flet ((read-int ()
           (loop :with n := 0
                 :for c := (read-char stream)
                 :for d := (digit-char-p c)
                 :if d :do
                   (setf n (+ (* 10 n) d))
                 :else :do
                   (unread-char c stream)
                   (return n))))
    (let* ((ts (prog1 (read-int) (read-char stream)))
           (id (make-array 28 :element-type 'base-char))
           (_ (progn (read-sequence id stream :end 28)
                     (read-char stream)))
           (x (prog1 (read-int) (read-char stream)))
           (y (prog1 (read-int) (read-char stream)))
           (col (prog1 (read-int) (read-char stream))))
      (declare (ignore _))
      (make-csv-record :timestamp ts :user-id id :x x :y y :color col))))

(defmacro with-place-colors-cl-gd ((color-indexes) &body body)
  `(let ((,color-indexes
           (list ,@(mapcar (lambda (rgb)
                             `(allocate-color ,(ldb (byte 8 16) rgb)
                                              ,(ldb (byte 8 8) rgb)
                                              ,(ldb (byte 8 0) rgb)))
                           '(#xffffff #xe4e4e4 #x888888 #x222222
                             #xffa7d1 #xe50000 #xe59500 #xa06a42
                             #xe5d900 #x94e044 #x02be01 #x00e5f0
                             #x0083c7 #x0000ea #xe04aff #x820080)))))
     ,@body))

(defun make-board ()
  (make-array '(1001 1001)
              :element-type '(unsigned-byte 4)
              :initial-element 0))

(defun make-place-gif-cl-gd (time-per-frame
                             &key (frame-rate 30) (in "tile_placements.csv") (out "place.gif"))
  (assert (> frame-rate 0))

  (with-open-file (stream in)
    (read-line stream) ;discard header row

    (with-image* (1001 1001)
      (with-place-colors-cl-gd (colors)
        (allocate-color 0 0 0 :alpha 0)
        (with-animated-gif (out :global-color-map-p t :default-delay (ceiling (* 100 (/ 1 frame-rate))))
          (loop :with last-frame
                :with scratch-frame := (create-image 1001 1001)
                :with last-frame-start-ts
                :while (peek-char nil stream nil nil)
                :do (destructuring-bind (ts nil x y color-code) (parse-csv-line stream)
                      (cond ((not last-frame-start-ts)
                             ;; Initial frame
                             (set-pixel x y :color (nth color-code colors) :image scratch-frame)
                             (setq last-frame-start-ts ts))

                            ((> (- ts last-frame-start-ts) time-per-frame)
                             ;; Add current frame to animation, capture timestamp
                             (setq last-frame
                                   (add-image-to-animation scratch-frame :last-image last-frame))
                             (setq last-frame-start-ts ts))

                            (t
                             ;; Usual case, update current frame
                             (set-pixel x y :color (nth color-code colors) :image scratch-frame))))))))))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :skippy))

(in-package :skippy)

(defstruct csv-record
  ;; Author: Elijah Malaby
  (timestamp 0 :type (integer 0))
  (user-id "" :type string)
  (x 0 :type integer)
  (y 0 :type integer)
  (color 0 :type integer))

(defun parse-csv-line (stream)
  ;; Author: Elijah Malaby
  (flet ((read-int ()
           (loop :with n := 0
                 :for c := (read-char stream)
                 :for d := (digit-char-p c)
                 :if d :do
                   (setf n (+ (* 10 n) d))
                 :else :do
                   (unread-char c stream)
                   (return n))))
    (let* ((ts (prog1 (read-int) (read-char stream)))
           (id (make-array 28 :element-type 'base-char))
           (_ (progn (read-sequence id stream :end 28)
                     (read-char stream)))
           (x (prog1 (read-int) (read-char stream)))
           (y (prog1 (read-int) (read-char stream)))
           (col (prog1 (read-int) (read-char stream))))
      (declare (ignore _))
      (make-csv-record :timestamp ts :user-id id :x x :y y :color col))))

(defmacro with-place-colors-skippy ((color-indexes color-table) &body body)
  `(let ((,color-indexes (list ,@(mapcar (lambda (color)
                                           `(ensure-color ,color ,color-table))
                                         '(#xffffff #xe4e4e4 #x888888 #x222222
                                           #xffa7d1 #xe50000 #xe59500 #xa06a42
                                           #xe5d900 #x94e044 #x02be01 #x00e5f0
                                           #x0083c7 #x0000ea #xe04aff #x820080)))))
     ,@body))

(defun make-place-gif-skippy (time-per-frame
                              &key (frame-rate 30) (in "tile_placements.csv") (out "place.gif"))
  (assert (> frame-rate 0))

  (with-open-file (stream in)
    (read-line stream) ;discard header row

    (let* ((data-stream (make-data-stream :height 1001 :width 1001 :color-table t :loopingp t))
           (color-table (make-color-table)))
      (with-place-colors-skippy (colors color-table)
        (let ((white (ensure-color #xffffff collor-table))
              (frame (make-image :data-stream data-stream :width 1001 :height 1001
                                 :image-data (make-image-data 1001 1001 :initial-element white))))
          )))))
