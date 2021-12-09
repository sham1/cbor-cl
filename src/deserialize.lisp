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
  (let* ((len (get-additional-info additional-info stream))
	 (arr (make-array len :element-type '(unsigned-byte 8) :initial-element 0)))
    (dotimes (i len)
      (setf (aref arr i) (read-byte stream)))
    arr))

(defun deserialize-bignum (stream)
  (let* ((lead-byte (read-byte stream))
	 (additional-info (logand lead-byte #b11111))
	 (arr (deserialize-byte-string additional-info stream)))
    (reduce (lambda (acc b) (logior (ash acc 8) b)) arr :initial-value 0)))

(defun deserialize-tagged (additional-info stream)
  (let ((tag-val (deserialize-uint additional-info stream)))
    (cond ; TODO: Support all tag types
      ((= tag-val +tag-unsigned-bignum+) (deserialize-bignum stream))
      ((= tag-val +tag-negative-bignum+) (- -1 (deserialize-bignum stream))))))

(defun deserialize-simple-value (additional-info stream)
  (let ((value (deserialize-uint additional-info stream)))
    (cond
      ((= value 20) +false+)
      ((= value 21) +true+)
      ((= value 22) +null+)
      ((= value 23) +undefined+)
      (t (error 'invalid-message)))))

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

(defun deserialize (stream)
  "Reads a CBOR value from ``stream'' and returns it or signals a ``deserialization-error''"
  (let* ((lead-byte (read-byte stream))
	 (major-type (ash lead-byte -5))
	 (additional-info (logand lead-byte #b11111)))
    (cond
      ((= major-type +major-type-uint+) (deserialize-uint additional-info stream))
      ((= major-type +major-type-nint+) (deserialize-negint additional-info stream))
      ((= major-type +major-type-tag+) (deserialize-tagged additional-info stream))
      ((= major-type +major-type-simple/float+) (deserialize-simple/float additional-info stream))
      (t (error 'unknown-major-type :major-type major-type)))))
