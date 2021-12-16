(defpackage cbor-cl
  (:use :cl)
  (:import-from #:nibbles
		#:read-ub16/be
		#:read-ub32/be
		#:read-ub64/be
		#:write-ub16/be
		#:write-ub32/be
		#:write-ub64/be)
  (:import-from #:babel
		#:string-to-octets
		#:octets-to-string)
  (:export #:serialize
	   #:deserialize
	   #:+true+
	   #:+false+
	   #:+null+
	   #:+undefined+

	   #:simple-value
	   #:simple-value-value

	   #:base64url-data
	   #:base64url-data-content

	   #:base64-data
	   #:base64-data-content

	   #:base16-data
	   #:base16-data-content

	   #:nested-cbor
	   #:nested-cbor-data))
