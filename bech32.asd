(asdf:defsystem :bech32
  :name "bech32"
  :description "A library for encoding and decoding bech32 strings"
  :author "Anton Kovalenko <anton@sw4me.com>"
  :licence "Public Domain"
  :serial t
  :default-component-class cl-source-file
  :depends-on ("alexandria"
	       "babel")
  :components
  ((:static-file "README.md")
   (:file "bech32"))
  :in-order-to ((test-op (test-op :bech32/test))))

(asdf:defsystem :bech32/test
  :depends-on ("bech32"
	       "fiveam")
  :serial t
  :default-component-class cl-source-file
  :components
  ((:file "bech32-test"))
  :perform (test-op (o c)
                    (uiop:symbol-call :fiveam '#:run! :bech32)))
