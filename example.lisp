(defpackage #:bech32/examples
  (:use #:common-lisp
	#:bech32))

(in-package #:bech32/examples)

(defun decode-lnurl (string)
  (multiple-value-bind (hrp data)
      (decode string)
    (assert (string-equal "lnurl" hrp))
    (babel:octets-to-string
     (convert-bits data 5 8) :encoding :utf-8)))

(defun encode-lnurl (string)
  (encode "lnurl"
	  (convert-bits (babel:string-to-octets string) 8 5 t)))

(defun decode-address (string)
  (multiple-value-bind (hrp data)
      (decode string)
    (assert (member hrp '("bc" "tb") :test 'string=))
    (assert (plusp (length data)))
    (values (convert-bits (subseq data 1) 5 8)
	    (aref data 0)
	    hrp)))

(defun encode-address (hrp octets &key (version 0))
  (assert (member hrp '("bc" "tb") :test 'string=))
  (encode hrp (concatenate 'mod32vector (list version)
			    (convert-bits octets 8 5 t))))

