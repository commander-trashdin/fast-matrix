;;;; fast-matrix.asd

(asdf:defsystem #:fast-matrix
  :description "Optimizes order of matrix multiplication"
  :author "Andrew <aun.sokolov@gmail.com>"
  :license  "MIT"
  :version "0.5"
  :serial t
  :components ((:file "package")
               (:file "fast-matrix")))
