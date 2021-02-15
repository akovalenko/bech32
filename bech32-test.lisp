(defpackage #:bech32/test
  (:use #:common-lisp
	#:bech32
	#:fiveam))

(in-package #:bech32/test)

(def-suite :bech32)
(in-suite :bech32)

(defun decode-fails (string)
  "Return true if decoding STRING fails with BECH32-PARSE-ERROR"
  (handler-case (prog1 nil (decode string))
    (bech32-parse-error (e)
      (declare (ignorable e))
      t)))

;;; See https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki
;;; for test vectors.  Invalid bitcoin addresses are not checked
;;; because they're outside of scope of bech32 proper.

(def-test decode/bad ()
  
  (is (decode-fails " 1nwldj5")
      "HRP character out of range")
  
  (is (decode-fails
       (concatenate 'string (list (code-char 127))
		    "1axkwrx"))
      "HRP character out of range")
  
  (is (decode-fails "pzry9x0s0muk")
      "No separator character")

  (is (decode-fails "1pzry9x0s0muk")
      "Empty HRP")

  (is (decode-fails "x1b4n0q5v")
      "Invalid data character")

  (is (decode-fails "li1dgmt3")
      "Too short checksum")
  
  (is (decode-fails (concatenate 'string "de1lg7wt" (list (code-char 255))))
      "Invalid character in checksum")

  (is (decode-fails "A1G7SGD8")
      "checksum calculated with uppercase form of HRP")
  
  (is (decode-fails "10a06t8")
      "empty HRP")
  
  (is (decode-fails "1qzzfhee")
      "empty HRP")
  
  (is (decode-fails "bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t5")
      "invalid checksum")

  (is (decode-fails "tb1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3q0sL5k7")
      "mixed case"))

(def-test decode/good ()
  (is (decode "A12UEL5L"))
  (is (decode "a12uel5l"))
  (is (decode "an83characterlonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio1tt5tgs"))
  (is (decode "abcdef1qpzry9x8gf2tvdw0s3jn54khce6mua7lmqqqxw"))
  (is (decode "11qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqc8247j"))
  (is (decode "split1checkupstagehandshakeupstreamerranterredcaperred2y9e3w"))
  (is (decode "?1ezyfcl")))
