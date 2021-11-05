(defsystem "cbor-cl"
  :version "0.1.0"
  :author "Jani Juhani Sinervo"
  :license "MIT"
  :depends-on ("alexandria"
               "nibbles"
	       "babel")
  :components ((:module "src"
                :components
                ((:file "package")
		 (:file "constants" :depends-on ("package"))
		 (:file "deserialize" :depends-on ("package" "constants")))))
  :description ""
  :in-order-to ((test-op (test-op "cbor-cl/tests"))))

(defsystem "cbor-cl/tests"
  :author "Jani Juhani Sinervo"
  :license "MIT"
  :depends-on ("cbor-cl"
	       "flexi-streams"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "deserialize"))))
  :description "Test system for cbor-cl"
  :perform (test-op (op c) (symbol-call :rove :run c)))
