#|
 This file is a part of float-features
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem float-features-tests
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Tests for Float Features"
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :float-features-tests))
  :homepage "https://github.com/Shinmera/float-features"
  :serial T
  :components ((:file "test-float-features"))
  :depends-on (:float-features :parachute))
