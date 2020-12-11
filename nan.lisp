
(in-package #:org.shirakumo.float-features)

;; let's forget about long/short

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

