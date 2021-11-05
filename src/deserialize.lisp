(in-package :cbor-cl)

(define-condition deserialization-error (error) ())

(define-condition unknown-major-type (deserialization-error)
  ((major-type :accessor unknown-major-type-major-type))
  (:report
   (lambda (condition stream)
     (format stream "Unknown major type ~D.~&" (unknown-major-type-major-type condition)))))

(define-condition invalid-message (deserialization-error)
  ())

(defun deserialize-uint (additional-info stream)
  (cond
    ((< additional-info +additional-info-1-byte+) additional-info)  ; Is the uint encoded in the additional info
    ((= additional-info +additional-info-1-byte+) (read-byte stream))
    ((= additional-info +additional-info-2-byte+) (read-ub16/be stream))
    ((= additional-info +additional-info-4-byte+) (read-ub32/be stream))
    ((= additional-info +additional-info-8-byte+) (read-ub64/be stream))
    (t (error 'invalid-message))))

(defun deserialize (stream)
  "Reads a CBOR value from ``stream'' and returns it or signals a ``deserialization-error''"
  (let* ((lead-byte (read-byte stream))
	 (major-type (ash lead-byte -5))
	 (additional-info (logand lead-byte #b11111)))
    (cond
      ((= major-type +major-type-uint+) (deserialize-uint additional-info stream))
      (t (error 'unknown-major-type :major-type major-type)))))
