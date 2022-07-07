
(in-package #:org.shirakumo.float-features)

;; let's forget about long/short

(handler-case
    #-ecl
    (progn
      (bits-short-float 0)
      (defconstant short-float-nan
        (bits-short-float #b0111111000000000)))
    #+ecl
    (defconstant short-float-nan (coerce (ext:nan) 'short-float))
  (error ()
    (define-symbol-macro short-float-nan
        (bits-short-float #b0111111000000000))))

(handler-case
    #-ecl
    (progn
      (bits-single-float 0)
      (defconstant single-float-nan
        (bits-single-float #b01111111110000000000000000000000)))
    #+ecl
    (defconstant single-float-nan (coerce (ext:nan) 'single-float))
    (error ()
      (define-symbol-macro single-float-nan
          (bits-single-float #b01111111110000000000000000000000))))

(handler-case
    #-ecl
    (progn
      (bits-double-float 0)
      (defconstant double-float-nan
        (bits-double-float #b0111111111111000000000000000000000000000000000000000000000000000)))
    #+ecl
    (defconstant double-float-nan (coerce (ext:nan) 'double-float))
    (error ()
      (define-symbol-macro double-float-nan
          (bits-double-float #b0111111111111000000000000000000000000000000000000000000000000000))))

(handler-case
    #-ecl
    (progn
      (bits-long-float 0)
      (defconstant long-float-nan
        (bits-long-float #b01111111111111111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)))
    #+ecl
    (defconstant long-float-nan (coerce (ext:nan) 'long-float))
  (error ()
    (define-symbol-macro long-float-nan
        (bits-long-float #b01111111111111111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000))))

