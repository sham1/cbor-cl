(in-package :cbor-cl)

(define-condition deserialization-error (error) ())

(define-condition unknown-major-type (deserialization-error)
  ((major-type :accessor unknown-major-type-major-type
	       :initarg :major-type))
  (:report
   (lambda (condition stream)
     (format stream "Unknown major type ~D.~&" (unknown-major-type-major-type condition)))))

(define-condition invalid-message (deserialization-error)
  ())

(defvar *allow-arbitrary-length* t)

(defun get-additional-info (additional-info stream)
  (cond
    ((< additional-info +additional-info-1-byte+) additional-info)  ; Is the info encoded in the additional info
    ((= additional-info +additional-info-1-byte+) (read-byte stream)) ; Or is it afterwards
    ((= additional-info +additional-info-2-byte+) (read-ub16/be stream))
    ((= additional-info +additional-info-4-byte+) (read-ub32/be stream))
    ((= additional-info +additional-info-8-byte+) (read-ub64/be stream))
    (t (error 'invalid-message))))

(defun deserialize-uint (additional-info stream)
  (get-additional-info additional-info stream))

(defun deserialize-negint (additional-info stream)
  (- -1 (get-additional-info additional-info stream)))

(defun deserialize-byte-string (additional-info stream)
  (if (and *allow-arbitrary-length* (= additional-info +additional-info-no-value+))
      ;; Byte strings with unknown lengths
      (let* ((*allow-arbitrary-length* nil)
	     (strings nil)
	     (datum (deserialize-internal stream)))
	 (loop until (eql datum :break) do
	   (unless (subtypep (type-of datum) '(array (unsigned-byte 8)))
	     (error 'invalid-message))
	   (push datum strings)
	   (setf datum (deserialize-internal stream)))
	 (let ((ret (make-array 1 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
	   (loop for str in (nreverse strings) do
	     (loop for byte across str do
	       (vector-push-extend byte ret)))
	   ret))
      ;; Byte strings with known lengths
      (let* ((len (get-additional-info additional-info stream))
	     (arr (make-array len :element-type '(unsigned-byte 8) :initial-element 0)))
	(dotimes (i len)
	  (setf (aref arr i) (read-byte stream)))
	arr)))

(defun deserialize-bignum (stream)
  (let* ((lead-byte (read-byte stream))
	 (additional-info (logand lead-byte #b11111))
	 (arr (deserialize-byte-string additional-info stream)))
    (reduce (lambda (acc b) (logior (ash acc 8) b)) arr :initial-value 0)))

(defun deserialize-string (additional-info stream)
  (if (and *allow-arbitrary-length* (= additional-info +additional-info-no-value+))
      ;; Strings with unknown lengths
      (let* ((*allow-arbitrary-length* nil)
	     (datum (deserialize-internal stream))
	     (strings nil))
	(loop until (eql datum :break) do
	   (unless (typep datum 'string)
	     (error 'invalid-message))
	   (push datum strings)
	   (setf datum (deserialize-internal stream)))
	(format nil "~{~A~}" (nreverse strings)))
      ;; Strings with known lengths
      (let ((bytes (deserialize-byte-string additional-info stream)))
	(octets-to-string bytes :errorp t :encoding :utf-8))))

(defun deserialize-date-time (stream)
  (let* ((lead-byte (read-byte stream))
	 (additional-info (logand lead-byte #b11111))
	 (date (deserialize-string additional-info stream)))
    (local-time:parse-rfc3339-timestring date)))

(defun deserialize-epoch-time (stream)
  (let ((time (deserialize stream)))
    (unless (or (integerp time) (floatp time))
      (error 'invalid-message)) ; Epoch time has to be either integers or floats

    (let ((nsecs (* 1000000000 (nth-value 1 (floor time)))))
      (local-time:unix-to-timestamp (floor time) :nsec nsecs))))

(defun deserialize-inner-cbor (stream)
  (let ((byte-string (deserialize-byte-string (logand (read-byte stream)
						      #b11111)
					      stream)))
    (make-instance 'nested-cbor :data byte-string)))

(defun deserialize-hinted-byte-string (type stream)
  (let ((data (deserialize-byte-string
	       (logand (read-byte stream) #b11111)
	       stream)))
    (cond
      ((= type +tag-data-expected-base64url+)
       (make-instance 'base64url-data :content data))
      ((= type +tag-data-expected-base64+)
       (make-instance 'base64-data :content data))
      ((= type +tag-data-expected-base16+)
       (make-instance 'base16-data :content data)))))

(defun deserialize-array (additional-info stream)
  (if (= additional-info +additional-info-no-value+)
      ;; Arrays of unknown length
      (let ((datum (deserialize stream))
	    (arr (make-array 1 :fill-pointer 0)))
	(loop until (eql datum :break) do
	  (vector-push-extend datum arr)
	  (setf datum (deserialize stream)))
	arr)
      ;; Arrays of known length
      (let* ((len (get-additional-info additional-info stream))
	     (arr (make-array len)))
	(loop for i from 0 below len do
	  (setf (aref arr i) (deserialize stream)))
	arr)))

(defun deserialize-map (additional-info stream)
  (if (= additional-info +additional-info-no-value+)
      ;; Maps of unknown length
      (let ((datum (deserialize stream))
	    (map (make-hash-table :test #'equal)))
	(loop until (eql datum :break) do
	  (setf (gethash datum map) (deserialize stream))
	  (setf datum (deserialize stream)))
        map)
      ;; Maps of known length
      (let* ((len (get-additional-info additional-info stream))
	     (map (make-hash-table :test #'equal)))
	(dotimes (i len)
	  (let ((key (deserialize stream))
		(value (deserialize stream)))
	    (setf (gethash key map) value)))
	map)))

(defun deserialize-tagged (additional-info stream)
  (let ((tag-val (deserialize-uint additional-info stream)))
    (cond ; TODO: Support all tag types
      ((= tag-val +tag-date-time+) (deserialize-date-time stream))
      ((= tag-val +tag-epoch-time+) (deserialize-epoch-time stream))
      ((= tag-val +tag-unsigned-bignum+) (deserialize-bignum stream))
      ((= tag-val +tag-negative-bignum+) (- -1 (deserialize-bignum stream)))
      ((member tag-val `(,+tag-data-expected-base16+
			 ,+tag-data-expected-base64+
			 ,+tag-data-expected-base64url+)
	       :test #'=)
       (deserialize-hinted-byte-string tag-val stream))
      ((= tag-val +tag-encoded-cbor-datum+) (deserialize-inner-cbor stream))
      (t (error 'invalid-message)))))

(defun deserialize-simple-value (additional-info stream)
  (let ((value (deserialize-uint additional-info stream)))
    (cond
      ((= value 20) +false+)
      ((= value 21) +true+)
      ((= value 22) +null+)
      ((= value 23) +undefined+)
      (t (make-instance 'simple-value :value value)))))

(defun deserialize-simple/float (additional-info stream)
  (cond
    ((<= additional-info +additional-info-1-byte+) ; Simple values
     (deserialize-simple-value additional-info stream))

    ((= additional-info +additional-info-2-byte+) ; TODO: Support floats
     nil)
    ((= additional-info +additional-info-4-byte+) ; TODO: Support floats
     nil)
    ((= additional-info +additional-info-8-byte+) ; TODO: Support floats
     nil)

    ((= additional-info +additional-info-no-value+)
     :break)
    (t (error 'invalid-message))))

(defun deserialize-internal (stream)
  (let* ((lead-byte (read-byte stream))
	   (major-type (ash lead-byte -5))
	   (additional-info (logand lead-byte #b11111)))
      (cond
	((= major-type +major-type-uint+) (deserialize-uint additional-info stream))
	((= major-type +major-type-nint+) (deserialize-negint additional-info stream))
	((= major-type +major-type-octet-str+) (deserialize-byte-string additional-info stream))
	((= major-type +major-type-str+) (deserialize-string additional-info stream))
	((= major-type +major-type-seq+) (deserialize-array additional-info stream))
	((= major-type +major-type-map+) (deserialize-map additional-info stream))
	((= major-type +major-type-tag+) (deserialize-tagged additional-info stream))
	((= major-type +major-type-simple/float+) (deserialize-simple/float additional-info stream))
	(t (error 'unknown-major-type :major-type major-type)))))

(defun deserialize (stream)
  "Reads a CBOR value from ``stream'' and returns it or signals a ``deserialization-error''"
  (let ((*allow-arbitrary-length* t))
    (deserialize-internal stream)))
