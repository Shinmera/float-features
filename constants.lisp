(in-package #:org.shirakumo.float-features)

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

(handler-case
    (progn
      (bits-short-float 0)
      (defconstant short-float-nan
        (bits-short-float #b0111111000000000)))
  (error ()
    (define-symbol-macro short-float-nan
        (bits-short-float #b0111111000000000))))

(handler-case
    (progn
      (bits-single-float 0)
      (defconstant single-float-nan
        (bits-single-float #b01111111110000000000000000000000)))
  (error ()
    (define-symbol-macro single-float-nan
        (bits-single-float #b01111111110000000000000000000000))))

(handler-case
    (progn
      (bits-double-float 0)
      (defconstant double-float-nan
        (bits-double-float #b0111111111111000000000000000000000000000000000000000000000000000)))
  (error ()
    (define-symbol-macro double-float-nan
        (bits-double-float #b0111111111111000000000000000000000000000000000000000000000000000))))

(handler-case
    (progn
      (bits-long-float 0)
      (defconstant long-float-nan
        (bits-long-float #b01111111111111111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)))
  (error ()
    (define-symbol-macro long-float-nan
        (bits-long-float #b01111111111111111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000))))

