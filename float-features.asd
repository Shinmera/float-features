#|
 This file is a part of float-features
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem float-features
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A portability library for IEEE float features not covered by the CL standard."
  :homepage "https://github.com/Shinmera/float-features"
  :serial T
  :components ((:file "float-features")
               (:file "documentation")
               (:file "nan"))
  :in-order-to ((asdf:test-op (asdf:test-op :float-features-tests)))
  :depends-on (:documentation-utils))
