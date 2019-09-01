#|
 This file is a part of float-features
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:float-features
  (:nicknames #:org.shirakumo.float-features)
  (:use #:cl)
  (:export
   #:short-float-positive-infinity
   #:short-float-negative-infinity
   #:single-float-positive-infinity
   #:single-float-negative-infinity
   #:double-float-positive-infinity
   #:double-float-negative-infinity
   #:long-float-positive-infinity
   #:long-float-negative-infinity
   #:float-infinity-p
   #:float-nan-p
   #:with-float-traps-masked
   #:short-float-bits
   #:single-float-bits
   #:double-float-bits
   #:long-float-bits
   #:bits-short-float
   #:bits-single-float
   #:bits-double-float
   #:bits-long-float))

(in-package #:org.shirakumo.float-features)

(defconstant short-float-positive-infinity
  #+ccl 1S++0
  #+clasp ext:short-float-positive-infinity
  #+cmucl extensions:short-float-positive-infinity
  #+ecl ext:short-float-positive-infinity
  #+mkcl ext:short-float-positive-infinity
  #+sbcl sb-ext:short-float-positive-infinity
  #+lispworks 1S++0
  #-(or ccl clasp cmucl ecl mkcl sbcl lispworks)
  most-positive-short-float)

(defconstant short-float-negative-infinity
  #+ccl -1S++0
  #+clasp ext:short-float-negative-infinity
  #+cmucl extensions:short-float-negative-infinity
  #+ecl ext:short-float-negative-infinity
  #+mkcl ext:short-float-negative-infinity
  #+sbcl sb-ext:short-float-negative-infinity
  #+lispworks -1S++0
  #-(or ccl clasp cmucl ecl mkcl sbcl lispworks)
  most-negative-short-float)

(defconstant single-float-positive-infinity
  #+abcl extensions:single-float-negative-infinity
  #+allegro excl:*infinity-single*
  #+ccl 1F++0
  #+clasp ext:single-float-positive-infinity
  #+cmucl extensions:single-float-positive-infinity
  #+ecl ext:single-float-positive-infinity
  #+mkcl mkcl:single-float-positive-infinity
  #+sbcl sb-ext:single-float-positive-infinity
  #+lispworks 1F++0
  #-(or abcl allegro ccl clasp cmucl ecl mkcl sbcl lispworks)
  most-positive-single-float)

(defconstant single-float-negative-infinity
  #+abcl extensions:single-float-negative-infinity
  #+allegro excl:*negative-infinity-single*
  #+ccl -1F++0
  #+clasp ext:single-float-negative-infinity
  #+cmucl extensions:single-float-negative-infinity
  #+ecl ext:single-float-negative-infinity
  #+mkcl mkcl:single-float-negative-infinity
  #+sbcl sb-ext:single-float-negative-infinity
  #+lispworks -1F++0
  #-(or abcl allegro ccl clasp cmucl ecl mkcl sbcl lispworks)
  most-negative-single-float)

(defconstant double-float-positive-infinity
  #+abcl extensions:double-float-negative-infinity
  #+allegro excl:*infinity-double*
  #+ccl 1D++0
  #+clasp ext:double-float-positive-infinity
  #+cmucl extensions:double-float-positive-infinity
  #+ecl ext:double-float-positive-infinity
  #+mkcl mkcl:double-float-positive-infinity
  #+sbcl sb-ext:double-float-positive-infinity
  #+lispworks 1D++0
  #-(or abcl allegro ccl clasp cmucl ecl mkcl sbcl lispworks)
  most-positive-double-float)

(defconstant double-float-negative-infinity
  #+abcl extensions:double-float-negative-infinity
  #+allegro excl:*negative-infinity-double*
  #+ccl -1D++0
  #+clasp ext:double-float-negative-infinity
  #+cmucl extensions:double-float-negative-infinity
  #+ecl ext:double-float-negative-infinity
  #+mkcl mkcl:double-float-negative-infinity
  #+sbcl sb-ext:double-float-negative-infinity
  #+lispworks -1D++0
  #-(or abcl allegro ccl clasp cmucl ecl mkcl sbcl lispworks)
  most-negative-double-float)

(defconstant long-float-positive-infinity
  #+ccl 1L++0
  #+clasp ext:long-float-positive-infinity
  #+cmucl extensions:long-float-positive-infinity
  #+ecl ext:long-float-positive-infinity
  #+mkcl ext:long-float-positive-infinity
  #+sbcl sb-ext:long-float-positive-infinity
  #+lispworks 1L++0
  #-(or ccl clasp cmucl ecl mkcl sbcl lispworks)
  most-positive-long-float)

(defconstant long-float-negative-infinity
  #+ccl -1L++0
  #+clasp ext:long-float-negative-infinity
  #+cmucl extensions:long-float-negative-infinity
  #+ecl ext:long-float-negative-infinity
  #+mkcl ext:long-float-negative-infinity
  #+sbcl sb-ext:long-float-negative-infinity
  #+lispworks -1L++0
  #-(or ccl clasp cmucl ecl mkcl sbcl lispworks)
  most-negative-long-float)

(defun float-infinity-p (float)
  #+abcl (system:float-infinity-p float)
  #+allegro (excl:infinityp float)
  #+ccl (ccl::infinity-p float)
  #+clasp (ext:float-infinity-p float)
  #+cmucl (extensions:float-infinity-p float)
  #+ecl (ext:float-infinity-p float)
  #+sbcl (sb-ext:float-infinity-p float)
  #-(or abcl allegro ccl clasp cmucl ecl sbcl)
  (etypecase float
    (short-float (or (= float short-float-negative-infinity)
                     (= float short-float-positive-infinity)))
    (single-float (or (= float single-float-negative-infinity)
                      (= float single-float-positive-infinity)))
    (double-float (or (= float double-float-negative-infinity)
                      (= float double-float-positive-infinity)))
    (long-float (or (= float long-float-negative-infinity)
                    (= float long-float-positive-infinity)))))

(defun float-nan-p (float)
  #+abcl (system:float-nan-p float)
  #+allegro (excl:nanp float)
  #+ccl (and (ccl::nan-or-infinity-p float)
             (not (ccl::infinity-p float)))
  #+clasp (ext:float-nan-p float)
  #+cmucl (extensions:float-nan-p float)
  #+ecl (ext:float-nan-p float)
  #+sbcl (sb-ext:float-nan-p float)
  #+lispworks (sys::nan-p float)
  #-(or abcl allegro ccl clasp cmucl ecl sbcl lispworks)
  (/= float float))

(defun keep (list &rest keeps)
  (loop for item in list
        when (find item keeps)
        collect item))

(defmacro with-float-traps-masked (traps &body body)
  (let ((traps (etypecase traps
                 ((eql T) '(:underflow :overflow :inexact :invalid :divide-by-zero :denormalized-operand))
                 (list traps))))
    #+abcl
    (let ((previous (gensym "PREVIOUS")))
      `(let ((,previous (extensions:get-floating-point-modes)))
         (unwind-protect
              (progn
                (extensions:set-floating-point-modes
                 :traps ',(keep traps :overflow :underflow))
                NIL ,@body)
           (apply #'extensions:set-floating-point-modes ,previous))))
    #+ccl
    (let ((previous (gensym "PREVIOUS"))
          (traps (loop for thing in traps
                       for trap = (case thing
                                    (:underflow :underflow)
                                    (:overflow :overflow)
                                    (:divide-by-zero :division-by-zero)
                                    (:invalid :invalid)
                                    (:inexact :inexact))
                       when trap collect trap)))
      `(let ((,previous (ccl:get-fpu-mode)))
         (unwind-protect
              (progn
                (ccl:set-fpu-mode
                 ,@(loop for trap in traps
                         collect trap collect NIL))
                NIL ,@body)
           (apply #'ccl:set-fpu-mode ,previous))))
    #+clisp
    (if (find :underflow)
        `(ext:without-floating-point-underflow
           ,@body)
        `(progn
           ,@body))
    #+cmucl
    `(extensions:with-float-traps-masked #+x86 ,traps #-x86 ,(remove :denormalized-operand traps)
       ,@body)
    #+ecl
    (let ((previous (gensym "PREVIOUS")))
      `(let ((,previous (si::trap-fpe :last T)))
         (unwind-protect
              (progn
                ,@(loop for trap in traps
                        for keyword = (case trap
                                        (:underlow :floating-point-underflow)
                                        (:overflow :floating-point-overflow)
                                        (:inexact :floating-point-inexact)
                                        (:invalid :floating-point-invalid)
                                        (:divide-by-zero :division-by-zero))
                        when keyword collect `(si::trap-fpe ,keyword T))
                NIL ,@body)
           (si::trap-fpe ,previous NIL))))
    #+sbcl
    `(sb-int:with-float-traps-masked #+x86 ,traps #-x86 ,(remove :denormalized-operand traps)
       ,@body)
    #-(or abcl ccl clisp cmucl ecl sbcl)
    (declare (ignore traps))
    #-(or abcl ccl clisp cmucl ecl sbcl)
    `(progn ,@body)))

(declaim (ftype (function (T) (unsigned-byte 16)) short-float-bits))
(defun short-float-bits (float)
  (declare (ignore float))
  (error "Implementation not supported."))

(declaim (ftype (function (T) (unsigned-byte 32)) single-float-bits))
(defun single-float-bits (float)
  #+abcl
  (system:single-float-bits float)
  #+allegro
  (multiple-value-bind (high low) (excl:single-float-to-shorts float)
    (logior low (ash high 32)))
  #+ccl
  (ccl::single-float-bits float)
  #+cmucl
  (kernel:single-float-bits float)
  #+lispworks
  (let ((v (sys:make-typed-aref-vector 4)))
    (declare (optimize (speed 3) (float 0) (safety 0)))
    (declare (dynamic-extent v))
    (setf (sys:typed-aref 'single-float v 0) float)
    (sys:typed-aref '(unsigned-byte 32) v 0))
  #+sbcl
  (sb-kernel:single-float-bits float)
  #-(or abcl allegro ccl cmucl lispworks sbcl)
  (error "Implementation not supported."))

(declaim (ftype (function (T) (unsigned-byte 64)) double-float-bits))
(defun double-float-bits (float)
  #+abcl
  (logior (system::double-float-low-bits float)
          (ash (system::double-float-high-bits float) 32))
  #+allegro
  (multiple-value-bind (s3 s2 s1 s0) (excl:double-float-to-shorts float)
    (logior s0 (ash s1 16) (ash s2 32) (ash s3 48)))
  #+ccl
  (multiple-value-bind (high low) (ccl::double-float-bits float)
    (logior low (ash high 32)))
  #+cmucl
  (logior (kernel:double-float-low-bits float)
          (ash (kernel:double-float-high-bits float) 32))
  #+lispworks
  (let ((v (sys:make-typed-aref-vector 8))
    (declare (optimize (speed 3) (float 0) (safety 0)))
    (declare (dynamic-extent v))
    (setf (sys:typed-aref 'double-float v 0) float)
        #+x86-64 (sys:typed-aref '(unsigned-byte 64) v 0)
        #-x64-64 (logior (sys:typed-aref '(unsigned-byte 32) v 0)
                         (ash (sys:typed-aref '(unsigned-byte 32) v 4) 32))))
  #+sbcl
  (logior (sb-kernel:double-float-low-bits float)
          (ash (sb-kernel:double-float-high-bits float) 32))
  #-(or abcl allegro ccl cmucl lispworks sbcl)
  (error "Implementation not supported."))

(declaim (ftype (function (T) (unsigned-byte 128)) long-float-bits))
(defun long-float-bits (float)
  (declare (ignore float))
  (error "Implementation not supported."))

(declaim (ftype (function (T) short-float) bits-short-float))
(defun bits-short-float (bits)
  (declare (ignore bits))
  (error "Implementation not supported."))

(declaim (ftype (function (T) single-float) bits-single-float))
(defun bits-single-float (bits)
  #+abcl
  (system:make-single-float bits)
  #+allegro
  (excl:shorts-to-single-float (ldb (byte 16 16) bits) (ldb (byte 16 0) bits))
  #+ccl
  (ccl::host-single-float-from-unsigned-byte-32 bits)
  #+cmucl
  (kernel:make-single-float bits)
  #+lispworks
  (let ((v (sys:make-typed-aref-vector 4)))
    (declare (optimize speed (float 0) (safety 0)))
    (declare (dynamic-extent v))
    (setf (sys:typed-aref '(unsigned-byte 32) v 0) bits)
    (sys:typed-aref 'single-float v 0))
  #+sbcl
  (sb-kernel:make-single-float bits)
  #-(or abcl allegro ccl cmucl lispworks sbcl)
  (error "Implementation not supported."))

(declaim (ftype (function (T) double-float) bits-double-float))
(defun bits-double-float (bits)
  #+abcl
  (system:make-double-float bits)
  #+allegro
  (excl:shorts-to-double-float
   (ldb (byte 16 48) bits) (ldb (byte 16 32) bits) (ldb (byte 16 16) bits) (ldb (byte 16 0) bits))
  #+ccl
  (ccl:double-float-from-bits (ldb (byte 32 32) bits) (ldb (byte 32 0) bits))
  #+cmucl
  (kernel:make-double-float (ldb (byte 32 32) bits) (ldb (byte 32 0) bits))
  #+lispworks
  (let ((v (sys:make-typed-aref-vector 8)))
    (declare (optimize speed (float 0) (safety 0)))
    (declare (dynamic-extent v))
    #+x86-64 (setf (sys:typed-aref '(unsigned-byte 64) v 0) bits)
    #-x86-64 (setf (sys:typed-aref '(unsigned-byte 32) v 0) (ldb (byte 32 0) bits)
                   (sys:typed-aref '(unsigned-byte 32) v 4) (ldb (byte 32 32) bits))
    (sys:typed-aref 'double-float v 0))
  #+sbcl
  (sb-kernel:make-double-float (ldb (byte 32 32) bits) (ldb (byte 32 0) bits))
  #-(or abcl allegro ccl cmucl lispworks sbcl)
  (error "Implementation not supported."))

(declaim (ftype (function (T) long-float) bits-long-float))
(defun bits-long-float (bits)
  (declare (ignore bits))
  (error "Implementation not supported."))
