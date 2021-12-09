(in-package :cbor-cl)

;;; Major types
(defconstant +major-type-uint+ #b000)
(defconstant +major-type-nint+ #b001)
(defconstant +major-type-octet-str+ #b010)
(defconstant +major-type-str+ #b011)
(defconstant +major-type-seq+ #b100)
(defconstant +major-type-map+ #b101)
(defconstant +major-type-tag+ #b110)
(defconstant +major-type-simple/float+ #b111) ; Also floats

;;; Additional information specials
(defconstant +additional-info-1-byte+ 24)
(defconstant +additional-info-2-byte+ 25)
(defconstant +additional-info-4-byte+ 26)
(defconstant +additional-info-8-byte+ 27)
(defconstant +additional-info-no-value+ 31)

;;; Tags
(defconstant +tag-date-time+ 0)
(defconstant +tag-epoch-time+ 1)
(defconstant +tag-unsigned-bignum+ 2)
(defconstant +tag-negative-bignum+ 3)
(defconstant +tag-decimal-fraction+ 4)
(defconstant +tag-bigfloat+ 5)
(defconstant +tag-data-expected-base64url+ 21)
(defconstant +tag-data-expected-base64+ 22)
(defconstant +tag-data-expected-base16+ 23)
(defconstant +tag-encoded-cbor-datum+ 24)
(defconstant +tag-uri+ 32)
(defconstant +tag-base64uri+ 33)
(defconstant +tag-base64+ 34)
(defconstant +tag-mime+ 36)
(defconstant +tag-self-described+ 55799)

;;; Special values
(defconstant +break+ #xFF) ; Major type 7, "additional information": 31

;;; Simple values
(defclass simple-value ()
  ((value :initarg :value
	  :accessor simple-value-value)))
(defmethod print-object ((object simple-value) stream)
  (with-slots (value) object
    (print-unreadable-object (object stream :type t)
      (format stream ":VALUE ~A" value))))

;;; User-exposed constants
(defconstant +true+ 't)
(defconstant +false+ nil)

(defconstant +null+ :null)
(defconstant +undefined+ :undefined)
