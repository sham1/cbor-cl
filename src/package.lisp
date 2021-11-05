(defpackage cbor-cl
  (:use :cl)
  (:import-from #:nibbles
		#:read-ub16/be
		#:read-ub32/be
		#:read-ub64/be
		#:write-ub64/be
		#:write-ub64/be
		#:write-ub64/be)
  (:import-from #:babel
		#:string-to-octets
		#:octets-to-string)
  (:export #:serialize
	   #:deserialize
	   #:+true+
	   #:+false+
	   #:+null+
	   #:+undefined+))
