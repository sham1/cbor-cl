(in-package :cbor-cl)

(defgeneric serialize (stream object)
  (:documentation "Serializes objects into CBOR"))

(defun encode-type-info-additional-info (type additional-info stream)
  (cond
    ((< additional-info +additional-info-1-byte+) (write-byte (logior (ash type 5)
								      additional-info)
							      stream))
    ((<= 24 additional-info 255) (write-byte (logior (ash type 5) +additional-info-1-byte+) stream)
     (write-byte additional-info stream))
    ((<= 256 additional-info (1- (expt 2 16))) (write-byte (logior (ash type 5) +additional-info-2-byte+) stream)
     (write-ub16/be additional-info stream))
    ((<= (expt 2 16) additional-info (1- (expt 2 32))) (write-byte (logior (ash type 5) +additional-info-4-byte+) stream)
     (write-ub32/be additional-info stream))
    ((<= (expt 2 32) additional-info (1- (expt 2 64))) (write-byte (logior (ash type 5) +additional-info-8-byte+) stream)
     (write-ub64/be additional-info stream))))

(defun serialize-tag (tag stream)
  (encode-type-info-additional-info +major-type-tag+ tag stream))

(defun bignum-to-byte-array (num)
  (let ((bytes nil))
    (loop while (> num 0) do
      (push (logand num #xff) bytes)
      (setf num (ash num -8)))
    (let ((byte-array (make-array (length bytes) :element-type '(unsigned-byte 8))))
      (loop for byte in bytes
	    for i from 0 do
	      (setf (aref byte-array i) byte))
      byte-array)))

(defmethod serialize (stream (object array))
  (cond
    ((subtypep (array-element-type object) '(unsigned-byte 8))
     (encode-type-info-additional-info +major-type-octet-str+ (length object) stream)
     (write-sequence object stream))))

(defmethod serialize (stream (object integer))
  (cond
    ;; Non-negatives
    ((>= object (expt 2 64))
     (serialize-tag +tag-unsigned-bignum+ stream)
     (serialize stream (bignum-to-byte-array object)))
    ((>= object 0) (encode-type-info-additional-info +major-type-uint+ object stream))

    ;; Negatives
    ((<= object (- -1 (expt 2 64)))
     (serialize-tag +tag-negative-bignum+ stream)
     (serialize stream (bignum-to-byte-array (- (1+ object)))))
    (t (encode-type-info-additional-info +major-type-nint+ (- (1+ object)) stream))))
