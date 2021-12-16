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

(defmethod serialize (stream (object integer))
  (encode-type-info-additional-info +major-type-uint+ object stream))
