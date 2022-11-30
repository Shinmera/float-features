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
   #:short-float-nan
   #:single-float-positive-infinity
   #:single-float-negative-infinity
   #:single-float-nan
   #:double-float-positive-infinity
   #:double-float-negative-infinity
   #:double-float-nan
   #:long-float-positive-infinity
   #:long-float-negative-infinity
   #:long-float-nan
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

(eval-when (:compile-toplevel :load-toplevel)
  #+ecl
  (when (find-symbol "BITS-SINGLE-FLOAT" "SYSTEM") (pushnew :ecl-float-bit-translations *features*)))

(defconstant short-float-positive-infinity
  #+ccl 1S++0
  #+clasp ext:short-float-positive-infinity
  #+cmucl extensions:short-float-positive-infinity
  #+ecl ext:short-float-positive-infinity
  #+mezzano mezzano.extensions:short-float-positive-infinity
  #+mkcl ext:short-float-positive-infinity
  #+sbcl sb-ext:short-float-positive-infinity
  #+lispworks 1S++0
  #+allegro (coerce excl:*infinity-single* 'short-float)
  #-(or ccl clasp cmucl ecl mezzano mkcl sbcl lispworks allegro)
  most-positive-short-float)

(defconstant short-float-negative-infinity
  #+ccl -1S++0
  #+clasp ext:short-float-negative-infinity
  #+cmucl extensions:short-float-negative-infinity
  #+ecl ext:short-float-negative-infinity
  #+mezzano mezzano.extensions:short-float-negative-infinity
  #+mkcl ext:short-float-negative-infinity
  #+sbcl sb-ext:short-float-negative-infinity
  #+lispworks -1S++0
  #+allegro (coerce excl:*negative-infinity-single* 'short-float)
  #-(or ccl clasp cmucl ecl mezzano mkcl sbcl lispworks allegro)
  most-negative-short-float)

(defconstant single-float-positive-infinity
  #+abcl extensions:single-float-positive-infinity
  #+allegro excl:*infinity-single*
  #+ccl 1F++0
  #+clasp ext:single-float-positive-infinity
  #+cmucl extensions:single-float-positive-infinity
  #+ecl ext:single-float-positive-infinity
  #+mezzano mezzano.extensions:single-float-positive-infinity
  #+mkcl mkcl:single-float-positive-infinity
  #+sbcl sb-ext:single-float-positive-infinity
  #+lispworks 1F++0
  #-(or abcl allegro ccl clasp cmucl ecl mezzano mkcl sbcl lispworks)
  most-positive-single-float)

(defconstant single-float-negative-infinity
  #+abcl extensions:single-float-negative-infinity
  #+allegro excl:*negative-infinity-single*
  #+ccl -1F++0
  #+clasp ext:single-float-negative-infinity
  #+cmucl extensions:single-float-negative-infinity
  #+ecl ext:single-float-negative-infinity
  #+mezzano mezzano.extensions:single-float-negative-infinity
  #+mkcl mkcl:single-float-negative-infinity
  #+sbcl sb-ext:single-float-negative-infinity
  #+lispworks -1F++0
  #-(or abcl allegro ccl clasp cmucl ecl mezzano mkcl sbcl lispworks)
  most-negative-single-float)

(defconstant double-float-positive-infinity
  #+abcl extensions:double-float-positive-infinity
  #+allegro excl:*infinity-double*
  #+ccl 1D++0
  #+clasp ext:double-float-positive-infinity
  #+cmucl extensions:double-float-positive-infinity
  #+ecl ext:double-float-positive-infinity
  #+mezzano mezzano.extensions:double-float-positive-infinity
  #+mkcl mkcl:double-float-positive-infinity
  #+sbcl sb-ext:double-float-positive-infinity
  #+lispworks 1D++0
  #-(or abcl allegro ccl clasp cmucl ecl mezzano mkcl sbcl lispworks)
  most-positive-double-float)

(defconstant double-float-negative-infinity
  #+abcl extensions:double-float-negative-infinity
  #+allegro excl:*negative-infinity-double*
  #+ccl -1D++0
  #+clasp ext:double-float-negative-infinity
  #+cmucl extensions:double-float-negative-infinity
  #+ecl ext:double-float-negative-infinity
  #+mezzano mezzano.extensions:double-float-negative-infinity
  #+mkcl mkcl:double-float-negative-infinity
  #+sbcl sb-ext:double-float-negative-infinity
  #+lispworks -1D++0
  #-(or abcl allegro ccl clasp cmucl ecl mezzano mkcl sbcl lispworks)
  most-negative-double-float)

(defconstant long-float-positive-infinity
  #+ccl 1L++0
  #+clasp ext:long-float-positive-infinity
  #+cmucl extensions:long-float-positive-infinity
  #+ecl ext:long-float-positive-infinity
  #+mezzano mezzano.extensions:long-float-positive-infinity
  #+mkcl ext:long-float-positive-infinity
  #+sbcl sb-ext:long-float-positive-infinity
  #+lispworks 1L++0
  #-(or ccl clasp cmucl ecl mezzano mkcl sbcl lispworks)
  most-positive-long-float)

(defconstant long-float-negative-infinity
  #+ccl -1L++0
  #+clasp ext:long-float-negative-infinity
  #+cmucl extensions:long-float-negative-infinity
  #+ecl ext:long-float-negative-infinity
  #+mezzano mezzano.extensions:long-float-negative-infinity
  #+mkcl ext:long-float-negative-infinity
  #+sbcl sb-ext:long-float-negative-infinity
  #+lispworks -1L++0
  #-(or ccl clasp cmucl ecl mezzano mkcl sbcl lispworks)
  most-negative-long-float)

(declaim (inline float-infinity-p
                 float-nan-p))

(defun float-infinity-p (float)
  #+abcl (system:float-infinity-p float)
  #+allegro (excl:infinityp float)
  #+ccl (ccl::infinity-p float)
  #+clasp (ext:float-infinity-p float)
  #+cmucl (extensions:float-infinity-p float)
  #+ecl (ext:float-infinity-p float)
  #+mezzano (mezzano.extensions:float-infinity-p float)
  #+sbcl (sb-ext:float-infinity-p float)
  #-(or abcl allegro ccl clasp cmucl ecl mezzano sbcl)
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
  #+mezzano (mezzano.extensions:float-nan-p float)
  #+sbcl (sb-ext:float-nan-p float)
  #+lispworks (sys::nan-p float)
  #-(or abcl allegro ccl clasp cmucl ecl mezzano sbcl lispworks)
  (/= float float))

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
                 :traps ',(intersection traps '(:overflow :underflow)))
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
      `(let ((,previous (si::trap-fpe 'last NIL)))
         (unwind-protect
              (progn
                ,@(loop for trap in traps
                        for keyword = (case trap
                                        (:underflow 'floating-point-underflow)
                                        (:overflow 'floating-point-overflow)
                                        (:inexact 'floating-point-inexact)
                                        (:invalid 'floating-point-invalid)
                                        (:divide-by-zero 'division-by-zero))
                        when keyword collect `(si::trap-fpe ,keyword NIL))
                NIL ,@body)
           (si::trap-fpe ,previous T))))
    #+clasp
     `(ext:with-float-traps-masked ,traps
       ,@body)
    #+mezzano
    (let ((previous (gensym "PREVIOUS"))
          (traps (loop for thing in traps
                       for trap = (case thing
                                    (:underflow :underflow)
                                    (:overflow :overflow)
                                    (:divide-by-zero :divide-by-zero)
                                    (:invalid :invalid-operation)
                                    (:inexact :precision)
                                    #+x86-64
                                    (:denormalized-operand :denormal-operand))
                       when trap collect trap)))
      `(let ((,previous (mezzano.runtime::get-fpu-mode)))
         (unwind-protect
              (progn
                (mezzano.runtime::set-fpu-mode
                 ,@(loop for trap in traps
                         collect trap collect T))
                NIL ,@body)
           (apply #'mezzano.runtime::set-fpu-mode ,previous))))
    #+sbcl
    `(sb-int:with-float-traps-masked #+x86 ,traps #-x86 ,(remove :denormalized-operand traps)
       ,@body)
    #-(or abcl ccl clasp clisp cmucl ecl mezzano sbcl)
    (declare (ignore traps))
    #-(or abcl ccl clasp clisp cmucl ecl mezzano sbcl)
    `(progn ,@body)))

(declaim (inline short-float-bits
                 single-float-bits
                 double-float-bits
                 long-float-bits
                 bits-short-float
                 bits-single-float
                 bits-double-float
                 bits-long-float))

(declaim (ftype (function (T) (unsigned-byte 32)) single-float-bits))
(defun single-float-bits (float)
  #+abcl
  (ldb (byte 32 0) (system:single-float-bits float))
  #+allegro
  (multiple-value-bind (high low) (excl:single-float-to-shorts float)
    (logior low (ash high 16)))
  #+ccl
  (ccl::single-float-bits float)
  #+clasp
  (ext:single-float-to-bits float)
  #+cmucl
  (ldb (byte 32 0) (kernel:single-float-bits float))
  #+ecl-float-bit-translations
  (si:single-float-bits float)
  #+lispworks
  (let ((v (sys:make-typed-aref-vector 4)))
    (declare (optimize (speed 3) (float 0) (safety 0)))
    (declare (dynamic-extent v))
    (setf (sys:typed-aref 'single-float v 0) float)
    (sys:typed-aref '(unsigned-byte 32) v 0))
  #+mezzano
  (mezzano.extensions:single-float-to-ieee-binary32 float)
  #+sbcl
  (ldb (byte 32 0) (sb-kernel:single-float-bits float))
  #-(or abcl allegro ccl clasp cmucl ecl-float-bit-translations lispworks mezzano sbcl)
  (progn float (error "Implementation not supported.")))

(declaim (ftype (function (T) (unsigned-byte 16)) short-float-bits))
(defun short-float-bits (float)
  (declare (ignorable float))
  #+mezzano
  (mezzano.extensions:short-float-to-ieee-binary16 float)
  #+(or ecl sbcl cmucl allegro ccl
        (and 64-bit lispworks))
  (let* ((bits (single-float-bits float))
         (sign (ldb (byte 1 31) bits))
         (exp (- (ldb (byte 8 23) bits) 127))
         (sig (ldb (byte 23 0) bits)))
    (cond
      ((or (eql 0s0 float)
           (< exp -24))
       ;;underflow
       (ash sign 15))
      ((< exp -14)
       ;; encode as denormal if possible
       (logior (ash sign 15)
               0
               (ash (ldb (byte 11 13)
                         (logior (ash 1 23) sig))
                    (+ exp 14))))
      ((< exp 16)
       ;; encode directly
       (logior (ash sign 15)
               (ash (+ exp 15) 10)
               (ash sig -13)))
      ((zerop sig)
       ;; infinity
       (if (zerop sign)
           #b0111110000000000
           #b1111110000000000))
      (t
       ;;NaN
       (logior (ash sign 15)
               (ash #x1f 10)
               (ldb (byte 10 13) sig)))))
  ;; clisp short-float is 1+8+16
  ;; 32bit lispworks 5+ is 1+8+??, lw4 only has double
  ;; not sure about others?
  #- (or mezzano ecl sbcl cmucl allegro ccl (and 64-bit lispworks))
  (progn float (error "Implementation not supported.")))

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
  #+clasp
  (ext:double-float-to-bits float)
  #+cmucl
  (ldb (byte 64 0)
   (logior (kernel:double-float-low-bits float)
           (ash (kernel:double-float-high-bits float) 32)))
  #+ecl-float-bit-translations
  (si:double-float-bits float)
  #+lispworks
  (let ((v (sys:make-typed-aref-vector 8)))
    (declare (optimize (speed 3) (float 0) (safety 0)))
    (declare (dynamic-extent v))
    (setf (sys:typed-aref 'double-float v 0) float)
        #+x86-64 (sys:typed-aref '(unsigned-byte 64) v 0)
        #-x64-64 (logior (sys:typed-aref '(unsigned-byte 32) v 0)
                         (ash (sys:typed-aref '(unsigned-byte 32) v 4) 32)))
  #+mezzano
  (mezzano.extensions:double-float-to-ieee-binary64 float)
  #+sbcl
  (ldb (byte 64 0)
       (logior (sb-kernel:double-float-low-bits float)
               (ash (sb-kernel:double-float-high-bits float) 32)))
  #-(or abcl allegro ccl clasp cmucl ecl-float-bit-translations lispworks mezzano sbcl)
  (progn float (error "Implementation not supported.")))

(declaim (ftype (function (T) (unsigned-byte 128)) long-float-bits))
(defun long-float-bits (float)
  (declare (ignorable float))
  #+ecl-float-bit-translations
  (si:long-float-bits float)
  #-(or ecl-float-bit-translations)
  (error "Implementation not supported."))

(declaim (ftype (function (T) single-float) bits-single-float))
(defun bits-single-float (bits)
  #+abcl
  (system:make-single-float bits)
  #+allegro
  (excl:shorts-to-single-float (ldb (byte 16 16) bits) (ldb (byte 16 0) bits))
  #+ccl
  (ccl::host-single-float-from-unsigned-byte-32 bits)
  #+clasp
  (ext:bits-to-single-float bits)
  #+cmucl
  (flet ((s32 (x)
           (logior x (- (mask-field (byte 1 31) x))) ))
    (kernel:make-single-float (s32 bits)))
  #+ecl-float-bit-translations
  (si:bits-single-float bits)
  #+lispworks
  (let ((v (sys:make-typed-aref-vector 4)))
    (declare (optimize speed (float 0) (safety 0)))
    (declare (dynamic-extent v))
    (setf (sys:typed-aref '(unsigned-byte 32) v 0) bits)
    (sys:typed-aref 'single-float v 0))
  #+mezzano
  (mezzano.extensions:ieee-binary32-to-single-float bits)
  #+sbcl
  (sb-kernel:make-single-float
   (sb-c::mask-signed-field 32 (the (unsigned-byte 32) bits)))
  #-(or abcl allegro ccl clasp cmucl ecl-float-bit-translations lispworks mezzano sbcl)
  (progn bits (error "Implementation not supported.")))
  
(declaim (ftype (function (T) short-float) bits-short-float))
(defun bits-short-float (bits)
  (declare (ignorable bits))
  #+mezzano
  (mezzano.extensions:ieee-binary16-to-short-float bits)
  #+(or ecl sbcl cmucl allegro ccl (and 64-bit lispworks))
  (let ((sign (ldb (byte 1 15) bits))
        (exp (ldb (byte 5 10) bits))
        (sig (ldb (byte 10 0) bits)))
    (if (= exp 31)
        (cond
          ((not (zerop sig))
           ;; NaNs
           (bits-single-float
            (logior (ash sign 31)
                    (ash #xff 23)
                    ;; store in high-bit to preserve quiet/signalling
                    (ash sig 13))))
          ;; infinities
          ((zerop sign)
           single-float-positive-infinity)
          (t
           single-float-negative-infinity))
        (cond
          ((= 0 exp sig)
           ;; +- 0
           (if (zerop sign) 0s0 -0s0))
          ((zerop exp)
           ;; denormals -> single floats
           (let ((d (- 11 (integer-length sig))))
             (setf exp (- -14 d))
             (setf sig (ldb (byte 11 0) (ash sig (1+ d))))
             (bits-single-float
              (logior (ash sign 31)
                      (ash (+ exp 127) 23)
                      (ash sig #.(- 23 11))))))
          (t
           ;; normal numbers
           (bits-single-float
            (logior (ash sign 31)
                    (ash (+ exp #.(+ 127 -15)) 23)
                    (ash sig #.(- 23 10))))))))
  #-(or allegro ccl cmucl ecl mezzano sbcl (and 64-bit lispworks))
  (progn bits (error "Implementation not supported.")))

(declaim (ftype (function (T) double-float) bits-double-float))
(defun bits-double-float (bits)
  #+abcl
  (system:make-double-float bits)
  #+allegro
  (excl:shorts-to-double-float
   (ldb (byte 16 48) bits) (ldb (byte 16 32) bits) (ldb (byte 16 16) bits) (ldb (byte 16 0) bits))
  #+ccl
  (ccl::double-float-from-bits (ldb (byte 32 32) bits) (ldb (byte 32 0) bits))
  #+clasp
  (ext:bits-to-double-float bits)
  #+cmucl
  (flet ((s32 (x)
           (logior x (- (mask-field (byte 1 31) x))) ))
    (kernel:make-double-float (s32 (ldb (byte 32 32) bits))
                              (ldb (byte 32 0) bits)))
  #+ecl-float-bit-translations
  (si:bits-double-float bits)
  #+lispworks
  (let ((v (sys:make-typed-aref-vector 8)))
    (declare (optimize speed (float 0) (safety 0)))
    (declare (dynamic-extent v))
    #+x86-64 (setf (sys:typed-aref '(unsigned-byte 64) v 0) bits)
    #-x86-64 (setf (sys:typed-aref '(unsigned-byte 32) v 0) (ldb (byte 32 0) bits)
                   (sys:typed-aref '(unsigned-byte 32) v 4) (ldb (byte 32 32) bits))
    (sys:typed-aref 'double-float v 0))
  #+mezzano
  (mezzano.extensions:ieee-binary64-to-double-float bits)
  #+sbcl
  (sb-kernel:make-double-float
   (sb-c::mask-signed-field 32 (ldb (byte 32 32) (the (unsigned-byte 64) bits)))
   (ldb (byte 32 0) bits))
  #-(or abcl allegro ccl clasp cmucl ecl-float-bit-translations lispworks mezzano sbcl)
  (progn bits (error "Implementation not supported.")))

(declaim (ftype (function (T) long-float) bits-long-float))
(defun bits-long-float (bits)
  (declare (ignorable bits))
  #+ecl-float-bit-translations
  (si:bits-long-float bits)
  #-(or ecl-float-bit-translations)
  (error "Implementation not supported."))
