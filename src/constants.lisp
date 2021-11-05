(in-package :cbor-cl)

;;; Major types
(defconstant +major-type-uint+ #b000)
(defconstant +major-type-sint+ #b001)
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

;;; Special values
(defconstant +break+ #xFF) ; Major type 7, "additional information": 31

;;; User-exposed constants
(defconstant +true+ 't)
(defconstant +false+ nil)

;; Has to be done this way so we can distinguish them from NIL
(defclass <null> () ())
(defclass <undefined> () ())

(defconstant +null+ (make-instance '<null>))
(defconstant +undefined+ (make-instance '<undefined>))
