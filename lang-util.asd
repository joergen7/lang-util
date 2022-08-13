(defsystem :lang-util
  :author      "Jörgen Brandt <joergen@cuneiform-lang.org>"
  :version     (:read-file-form "version.sexp")
  :description "Basic utilities for language models in Common Lisp"
  :license     "Apache License, Version 2.0"
  :pathname    "src/"
  :components  ((:file "package")
		(:file "lang-util" :depends-on ("package")))
  :in-order-to ((test-op (test-op :lang-util/test))))

(defsystem :lang-util/test
  :author      "Jörgen Brandt <joergen@cuneiform-lang.org>"
  :description "Test suite for lang-util"
  :license     "Apache License, Version 2.0"
  :depends-on  (:lang-util :fiveam)
  :pathname    "test/"
  :components  ((:file "package")
		(:file "lang-util-suite" :depends-on ("package"))
		(:file "test-lang-util"  :depends-on ("lang-util-suite")))
  :perform     (test-op (o c)
			(unless
			    (symbol-call :fiveam :run! (find-symbol* :lang-util-suite :lang-util/test))
			  (error "there were test failures"))))
