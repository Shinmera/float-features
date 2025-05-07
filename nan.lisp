(in-package #:org.shirakumo.float-features)

(handler-case
    (progn
      (bits-short-float 0)
      (defconstant SHORT-FLOAT-NAN
        (bits-short-float #b0111111000000000)))
  (error ()
    (define-symbol-macro SHORT-FLOAT-NAN
        (bits-short-float #b0111111000000000))))

(handler-case
    (progn
      (bits-single-float 0)
      (defconstant SINGLE-FLOAT-NAN
        (bits-single-float #b01111111110000000000000000000000)))
  (error ()
    (define-symbol-macro SINGLE-FLOAT-NAN
        (bits-single-float #b01111111110000000000000000000000))))

(handler-case
    (progn
      (bits-double-float 0)
      (defconstant DOUBLE-FLOAT-NAN
        (bits-double-float #b0111111111111000000000000000000000000000000000000000000000000000)))
  (error ()
    (define-symbol-macro DOUBLE-FLOAT-NAN
        (bits-double-float #b0111111111111000000000000000000000000000000000000000000000000000))))

(handler-case
    (progn
      (bits-long-float 0)
      (defconstant LONG-FLOAT-NAN
        (bits-long-float #b01111111111111111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)))
  (error ()
    (define-symbol-macro LONG-FLOAT-NAN
        (bits-long-float #b01111111111111111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000))))
