(asdf:defsystem "lazyk"
  :description "Lazy K interpreter"
  :author "youz"
  :license "MIT"
  :version "1.0"
  :components ((:file "lazyk"))
  :in-order-to ((test-op (test-op "lazyk.test"))))
