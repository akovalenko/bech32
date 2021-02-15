(defpackage #:bech32
  (:use #:common-lisp
	#:alexandria)
  (:export #:encode
	   #:decode
	   #:convert-bits
	   #:bech32-parse-error))

(in-package #:bech32)

(define-condition bech32-parse-error (simple-parse-error)
  ())

(define-constant +charset+
  "qpzry9x8gf2tvdw0s3jn54khce6mua7l"
  :test 'equal
  :documentation "bech32 characters")

(define-constant +generator+
    #(#x3b6a57b2 #x26508e6d #x1ea119fa #x3d4233dd #x2a1462b3)
  :test 'equalp
  :documentation "bech32 checksum generator")

(define-constant +generator-table+
    (coerce
     (loop for i below 32
	   collect (reduce 'logxor
			   (loop for j below 5
				 when (logbitp j i)
				   collect (aref +generator+ j))))
     '(simple-array (unsigned-byte 30) (32)))
  :test 'equalp
  :documentation "XOR combinations of +generator+ values for 5-bit selectors")

(define-constant +char-table+
    (coerce
     (loop for code from 0 to 127
	   collect (position (code-char code) +charset+ :test 'char-equal))
     'simple-vector)
  :test 'equalp
  :documentation "Base32 character values for characters coded 0-127")

(deftype mod32 ()
  "5-bit element of bech32 raw data array"
  '(mod 32))

(deftype mod32vector ()
  "Simple array of 5-bit elements"
  '(simple-array mod32 (*)))

(declaim (inline bech32-char-p
		 bech32-char
		 decode%))

(defun bech32-char-p (char)
  "Return char's bech32 value if it belongs to the bech32 charset,
NIL otherwise."
  (declare (optimize (speed 3)
		     (compilation-speed 0)))
  (let ((code (char-code char)))
    (and (<= 0 code 127)
	 (the (or (mod 32) null)
	      (svref +char-table+ code)))))

(defun bech32-char (value)
  "bech32 char corresponding to the VALUE"
  (declare (optimize (speed 3)
		     (compilation-speed 0)))
  (check-type value mod32)
  (char +charset+ value))

(defun unified-case-p (string)
  "Return true if string is either all-lowercase or all-uppercase"
  (loop
    with seen-lower-case = nil
    and seen-upper-case = nil
    for char across string
    when (lower-case-p char)
      do (if seen-upper-case (return)
	     (setf seen-lower-case t))
    else
      when (upper-case-p char)
	do (if seen-lower-case (return)
	       (setf seen-upper-case t))
    finally
       (return t)))

(defun polymod (bytes &optional start end initial-value)
  "Update INITIAL-VALUE of bech32 checksum (default 1)
using mod32vector's range of bytes [start; end)"
  (declare (optimize (speed 3)
		     (compilation-speed 0))
	   (type mod32vector bytes))
  
  (loop with checksum of-type (unsigned-byte 30) = (or initial-value 1)
	with start of-type array-index = (or start 0)
	with end of-type array-length = (or end (length bytes))
	for i from start below end
	for byte of-type mod32 = (aref bytes i)
	for top of-type mod32 = (ash checksum -25)
	do (setf checksum
		 (logxor (ash (logand checksum #x1ffffff) 5)
			 byte
			 (the (unsigned-byte 30)
			      (aref +generator-table+ top))))
	finally
	   (return checksum)))

(defmacro with-typecase ((variable &rest types) &body body)
  `(etypecase ,variable
       ,@(loop for type in types
	       collect `(,type ,@body))))

(defun verify-checksum (bytes)
  (= 1 (polymod bytes)))

(defun decode% (string &optional buffer)
  (declare (optimize (speed 3) (compilation-speed 0)))
  (check-type string string)
  (loop for char across string
	do (assert (<= 33 (char-code char) 126) ()
		   'bech32-parse-error
		   :format-control "Non-US-ASCII character in bech32: ~S"
		   :format-arguments (list char)))

  (assert (unified-case-p string) (string)
	  'bech32-parse-error
	  :format-control "Mixed case in bech32 string")
  (setf string (string-downcase string))
  (when buffer
    (check-type buffer (simple-array mod32 (*))))
  (let* ((hrp-size (or (position #\1 string :from-end t)
		       (error 'bech32-parse-error
			      :format-control "HRP not found")))
	 (mod32-size (+ hrp-size (length string)))
	 (buffer (or buffer (make-sequence 'mod32vector mod32-size))))
    (declare (type (simple-array mod32 (*)) buffer))
    (when (= 0 hrp-size)
      (error 'bech32-parse-error
	     :format-control "Zero-sized HRP"))
    
    (when (< (- (length string) 6) (1+ hrp-size))
      (error 'bech32-parse-error
	     :format-control "Checksum too short"))

    (with-typecase (string (simple-array character (*))
			   (simple-array base-char (*))
			   t)
      (when (< (length buffer) mod32-size)
	(setf buffer (make-sequence 'mod32vector mod32-size)))
      (dotimes (i hrp-size)
	(setf (aref buffer i)
	      (ldb (byte 3 5) (char-code (char string i)))))
      (setf (aref buffer hrp-size) 0)
      (dotimes (i hrp-size)
	(setf (aref buffer (+ hrp-size i 1))
	      (ldb (byte 5 0) (char-code (char string i)))))
      (loop for i from (+ hrp-size 1) below (length string)
	    do (setf (aref buffer (+ hrp-size i))
		     (let ((char (char string i)))
		       (or (bech32-char-p char)
			   (error 'bech32-parse-error
				  :format-control "Unexpected character: ~S"
				  :format-arguments (list char))))))
      (assert (verify-checksum buffer) ()
	      'bech32-parse-error
	      :format-control "Checksum mismatch")
      (values hrp-size
	      buffer
	      (1+ (* hrp-size 2))
	      (- (length buffer) 6)))))

(defun decode (string)
  (check-type string string)
  (multiple-value-bind (hrp-size buffer start end)
      (decode% string)
    (values (nstring-downcase (subseq string 0 hrp-size))
	    (subseq buffer start end))))

(defun encode (hrp sequence)
  "Get bech32-encoded value from human-readable part HRP and a SEQUENCE of
5-bit values."
  (let ((vector (coerce sequence 'mod32vector))
	(hrp (string-downcase hrp)))
    (declare (optimize (speed 3))
	     (type string hrp)
	     (type mod32vector vector))
    (let* ((hrp-size (length hrp))
	   (polymod 1)
	   (prefix (make-sequence 'mod32vector (1+ (* hrp-size 2)))))
      (declare (type (unsigned-byte 30) polymod))
      (setf (aref prefix hrp-size) 0)
      (dotimes (i hrp-size)
	(let ((code (char-code (char hrp i))))
	  (setf (aref prefix i)
		(ash code -5)
		(aref prefix (+ i 1 hrp-size))
		(logand 31 code))))
      
      (setf polymod (polymod prefix nil nil polymod))
      (setf polymod (polymod vector nil nil polymod))
      (setf polymod (polymod
		     (the mod32vector
			  (load-time-value
			   (make-sequence 'mod32vector 6 :initial-element 0)))
		     nil nil polymod))
      (let ((polymod (logxor 1 polymod)))
	(with-output-to-string (output)
	  (write-string hrp output)
	  (write-char #\1 output)
	  (loop for value across vector
		do (write-char (bech32-char value) output))
	  (loop for i below 6
		do (write-char (bech32-char
				(ldb (byte 5 (* 5 (- 5 i))) polymod))
			       output)))))))

(defun convert-bits (sequence from to &optional pad)
  "Rearrange bits from FROM-sized unsigned bytes in SEQUECE to TO-sized
bytes in output value, appending leftover bits if PAD is true,
discarding them (after ensuring they're zero) otherwise."
  (let* ((data 0)
	 (bits 0)
	 (source-bit-length (* (length sequence) from))
	 (result
	   (make-array (if pad
			   (ceiling source-bit-length to)
			   (floor source-bit-length to))
		       :element-type `(unsigned-byte ,to)))
	 (cursor 0))
    
    (loop for value across sequence
	  do (setf bits (+ bits from)
		   data (logior (ash data from) value))
	     (loop while (>= bits to)
		   do (decf bits to)
		      (setf (aref result (shiftf cursor (1+ cursor)))
			    (ldb (byte to bits) data)))
	     (setf data (ldb (byte bits 0) data))
	  finally
	     (unless pad
	       (assert (= 0 data) ()
		       'bech32-parse-error
		       :format-control "Non-zero padding in 8-to-5 conversion"))
	     (when (and pad (plusp bits))
	       (setf (aref result cursor)
		     (ash data (- to bits))))
	     (return result))))
