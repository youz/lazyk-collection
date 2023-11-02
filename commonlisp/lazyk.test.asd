(asdf:defsystem "lazyk.test"
  :description "Tests for Lazy K interpreter"
  :author "youz"
  :license "MIT"
  :version "1.0"
  :depends-on ("lazyk" "fiveam")
  :components ((:file "lazyk-test"))
  :perform (test-op (o c) (symbol-call :fiveam '#:run-all-tests)))
