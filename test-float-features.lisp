(in-package #:cl-user)

(defpackage #:float-features-tests
  (:nicknames #:org.shirakumo.float-features-tests)
  (:use #:cl))

;;; to run (parachute:test :float-features-tests)
(in-package #:float-features-tests)

(parachute:define-test float-features-divide-by-fp-zero-trapped
    :compile-at :compile-time
    (parachute:true
     (float-features:float-infinity-p
      (flet ((foo () (read-from-string "0.0"))
             (bar () (read-from-string "23")))
        (float-features:with-float-traps-masked (:divide-by-zero)
          (/ (bar) (foo)))))))

(parachute:define-test float-features-divide-by-fp-zero-non-trapped
    :compile-at :compile-time
    (parachute:true
     (handler-case
         (flet ((foo () (read-from-string "0.0"))
                (bar () (read-from-string "23")))
           (float-features:with-float-traps-masked ()
             (/ (bar) (foo))))
       (division-by-zero (error)
         (values t error)))))

(defun foo-float-features-1 (n)
  most-positive-long-float)

(defun bar-float-features-1 (n)
  most-positive-long-float)

(parachute:define-test float-features-overflow-trapped
    :compile-at :compile-time
    (parachute:true
     (float-features:float-infinity-p
      (float-features:with-float-traps-masked (:overflow :inexact)
        (let ((n (random 100)))
          (+ (foo-float-features-1 n) (bar-float-features-1 n)))))))

(parachute:define-test float-features-overflow-non-trapped
    :compile-at :compile-time
    (parachute:true
     (handler-case
         (let ((n (random 100)))
           (float-features:with-float-traps-masked ()
             (+ (foo-float-features-1 n) (bar-float-features-1 n))))
       (floating-point-overflow (error)
         (values t error)))))

(defun foo-float-features-2 (n)
  (- most-positive-long-float (read-from-string "3")))

(defun bar-float-features-2 (n)
  (- most-positive-long-float (read-from-string "3")))

(parachute:define-test float-features-overflow-or-inexact-trapped
    :compile-at :compile-time
    (parachute:true
     (float-features:float-infinity-p
      (float-features:with-float-traps-masked (:overflow :inexact)
        (let ((n (random 100)))
          (+ (foo-float-features-2 n) (bar-float-features-2 n)))))))

(parachute:define-test float-features-overflow-or-inexact-non-trapped
    :compile-at :compile-time
    (parachute:true
     (handler-case
         (let ((n (random 100)))
           (float-features:with-float-traps-masked ()
             (+ (foo-float-features-2 n) (bar-float-features-2 n))))
       (floating-point-inexact (error)
         (values t error))
       (floating-point-overflow (error)
         (values t error)))))

(defun foo-float-features-3 (n)
  (read-from-string "0.0"))

(defun bar-float-features-3 (n)
  (read-from-string "0.0"))

(parachute:define-test float-features-invalid-trapped
    :compile-at :compile-time
    (parachute:true
     (float-features:float-nan-p
      (float-features:with-float-traps-masked (:invalid)
        (let ((n (random 100)))
          (/ (foo-float-features-3 n) (bar-float-features-3 n)))))))

(parachute:define-test float-features-invalid-non-trapped
    :compile-at :compile-time
    (parachute:true
     (handler-case
         (float-features:with-float-traps-masked ()
           (let ((n (random 100)))
             (/ (foo-float-features-3 n) (bar-float-features-3 n))))
       (floating-point-inexact (error)
         (values t error))
       (division-by-zero (error)
         (values t error))
       (floating-point-invalid-operation (error)
         (values t error)))))

(parachute:define-test negative-bits-to-float
    :compile-at :compile-time
  (parachute:is = -1f0 (float-features:bits-single-float #xBF800000))
  (parachute:is = -1d0 (float-features:bits-double-float #xBFF0000000000000)))
