(in-package #:cl-user)

(defpackage #:float-features/tests
  (:nicknames #:org.shirakumo.float-features/tests)
  (:use #:cl))

;;; to run (parachute:test :float-features/tests)
(in-package #:float-features/tests)

(parachute:define-test float-features-1
    :compile-at :execute
    (parachute:true
     (float-features:float-infinity-p
      (flet ((foo () (if (> 10 (random 20)) 0.0 0.0))
             (bar () (if (> 10 (random 20)) 23 24)))
        (float-features:with-float-traps-masked (:divide-by-zero)
          (/ (bar) (foo)))))))

(parachute:define-test float-features-2
     :compile-at :execute
    (parachute:true
     (handler-case
         (flet ((foo () (if (> 10 (random 20)) 0.0 0.0))
                (bar () (if (> 10 (random 20)) 23 24)))
           (float-features:with-float-traps-masked ()
             (/ (bar) (foo))
             ))
       (division-by-zero (error)
         (values t error)))))

(defun foo-float-features-1 (n)
  (if (> n (random 20)) most-positive-long-float most-positive-long-float))

(defun bar-float-features-1 (n)
  (if (> n (random 20)) most-positive-long-float most-positive-long-float))

(parachute:define-test float-features-3
     :compile-at :execute
    (parachute:true
     (float-features:float-infinity-p
      (float-features:with-float-traps-masked (:overflow :inexact)
        (let ((n (random 100)))
          (+ (foo-float-features-1 n) (bar-float-features-1 n)))))))

(parachute:define-test float-features-4
     :compile-at :execute
    (parachute:true
     (handler-case
         (let ((n (random 100)))
           (float-features:with-float-traps-masked ()
             (+ (foo-float-features-1 n) (bar-float-features-1 n))))
       (FLOATING-POINT-OVERFLOW (error)
         (values t error)))))

(defun foo-float-features-2 (n)
  (if (> n (random 20)) (- most-positive-long-float 3) (- most-positive-long-float 3)))

(defun bar-float-features-2 (n)
  (if (> n (random 20)) (- most-positive-long-float 3) (- most-positive-long-float 3)))

(parachute:define-test float-features-5
     :compile-at :execute
    (parachute:true
     (float-features:float-infinity-p
      (float-features:with-float-traps-masked (:overflow :inexact)
        (let ((n (random 100)))
          (+ (foo-float-features-2 n) (bar-float-features-2 n)))))))

(parachute:define-test float-features-6
     :compile-at :execute
    (parachute:true
     (handler-case
         (let ((n (random 100)))
           (float-features:with-float-traps-masked ()
             (+ (foo-float-features-2 n) (bar-float-features-2 n))))
       (FLOATING-POINT-INEXACT (error)
         (values t error))
       (FLOATING-POINT-OVERFLOW (error)
         (values t error)))))

(defun foo-float-features-3 (n)
  (if (> n (random 20)) 0.0 0.0))

(defun bar-float-features-3 (n)
  (if (> n (random 20)) 0.0 0.0))

(parachute:define-test float-features-7
     :compile-at :execute
    (parachute:true
     (float-features:float-nan-p
      (float-features:with-float-traps-masked (:invalid)
        (let ((n (random 100)))
          (/ (foo-float-features-3 n) (bar-float-features-3 n)))))))

(parachute:define-test float-features-8
     :compile-at :execute
    (parachute:true
     (handler-case
         (float-features:with-float-traps-masked ()
           (let ((n (random 100)))
             (/ (foo-float-features-3 n) (bar-float-features-3 n))))
       (FLOATING-POINT-INEXACT (error)
         (values t error))
       (division-by-zero (error)
         (values t error))
       (FLOATING-POINT-INVALID-OPERATION (error)
         (values t error)))))
