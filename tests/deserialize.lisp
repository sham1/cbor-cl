(defpackage cbor-cl/tests/deserialize
  (:use
   :cl
   :cbor-cl
   :flexi-streams
   :rove))
(in-package :cbor-cl/tests/deserialize)

;;;; This tests the deserialization of CBOR data into the proper Lisp data structures.
;;;; All of the test-data is taken from RFC 8949.

(defun make-test-vector (vec)
  (let ((len (length vec)))
    (make-array len :element-type '(unsigned-byte 8) :initial-contents vec)))

(defun make-test-stream (vec)
  (make-in-memory-input-stream (make-test-vector vec)))

;; NOTE: To run this test file, execute `(asdf:test-system :cbor-cl)' in your Lisp.

(deftest test-deserialize-0
  (testing "should #(#x00) deserialize to 0"
    (ok (= 0 (deserialize (make-test-stream #(#x00)))))))

(deftest test-deserialize-1
  (testing "should #(#x01) deserialize to 1"
    (ok (= 1 (deserialize (make-test-stream #(#x01)))))))

(deftest test-deserialize-10
  (testing "should #(#x0a) deserialize to 10"
    (ok (= 10 (deserialize (make-test-stream #(#x0a)))))))

(deftest test-deserialize-23
  (testing "should #(#x17) deserialize to 23"
    (ok (= 23 (deserialize (make-test-stream #(#x17)))))))

(deftest test-deserialize-24
  (testing "should #(#x18 #x18) deserialize to 24"
    (ok (= 24 (deserialize (make-test-stream #(#x18 #x18)))))))

(deftest test-deserialize-25
  (testing "should #(#x18 #x19) deserialize to 25"
    (ok (= 25 (deserialize (make-test-stream #(#x18 #x19)))))))

(deftest test-deserialize-100
  (testing "should #(#x18 #x64) deserialize to 100"
    (ok (= 100 (deserialize (make-test-stream #(#x18 #x64)))))))

(deftest test-deserialize-1000
  (testing "should #(#x1a #x00 #x0f #x42 #x40) deserialize to 1,000"
    (ok (= 1000000 (deserialize (make-test-stream #(#x1a #x00 #x0f #x42 #x40)))))))

(deftest test-deserialize-1000000
  (testing "should #(#x1a #x00 #x0f #x42 #x40) deserialize to 1,000,000"
    (ok (= 1000000 (deserialize (make-test-stream #(#x1a #x00 #x0f #x42 #x40)))))))

(deftest test-deserialize-1000000000000
  (testing "should #(#x1b #x00 #x00 #x00 #xe8 #xd4 #xa5 #x10 #x00) deserialize to 1,000,000,000,000"
    (ok (= 1000000000000 (deserialize (make-test-stream #(#x1b #x00 #x00 #x00 #xe8 #xd4 #xa5 #x10 #x00)))))))

(deftest test-deserialize-18446744073709551615
  (testing "should #(#x1b #xff #xff #xff #xff #xff #xff #xff #xff) deserialize to 18446744073709551615"
    (ok (= 18446744073709551615 (deserialize (make-test-stream #(#x1b #xff #xff #xff #xff #xff #xff #xff #xff)))))))

(deftest test-deserialize-18446744073709551616
  (testing "should #(#xc2 #x49 #x01 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00) deserialize to 18446744073709551616"
    (ok (= 18446744073709551616 (deserialize (make-test-stream #(#xc2 #x49 #x01 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00)))))))

(deftest test-deserialize--18446744073709551616
  (testing "should #(#x3b #xff #xff #xff #xff #xff #xff #xff #xff) deserialize to -18446744073709551616"
    (ok (= -18446744073709551616 (deserialize (make-test-stream #(#x3b #xff #xff #xff #xff #xff #xff #xff #xff)))))))

(deftest test-deserialize--18446744073709551617
  (testing "should #(#xc3 #x49 #x01 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00) deserialize to -18446744073709551617"
    (ok (= -18446744073709551617 (deserialize (make-test-stream #(#xc3 #x49 #x01 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00)))))))

(deftest test-deserialize--1
  (testing "should #(#x20) deserialize to -1"
    (ok (= -1 (deserialize (make-test-stream #(#x20)))))))

(deftest test-deserialize--10
  (testing "should #(#x29) deserialize to -10"
    (ok (= -10 (deserialize (make-test-stream #(#x29)))))))

(deftest test-deserialize--100
  (testing "should #(#x38 #x63) deserialize to -100"
    (ok (= -100 (deserialize (make-test-stream #(#x38 #x63)))))))

(deftest test-deserialize--1000
  (testing "should #(#x39 #x03 #xe7) deserialize to -1000"
    (ok (= -1000 (deserialize (make-test-stream #(#x39 #x03 #xe7)))))))

;; TODO: Add support for floating-point

(deftest test-deserialize-false
  (testing "should #(#xf4) deserialize to nil"
    (ok (eql (deserialize (make-test-stream #(#xf4))) +false+))))

(deftest test-deserialize-true
  (testing "should #(#xf5) deserialize to t"
    (ok (eql (deserialize (make-test-stream #(#xf5))) +true+))))

(deftest test-deserialize-null
  (testing "should #(#xf6) deserialize to :null"
    (ok (eql (deserialize (make-test-stream #(#xf6))) +null+))))

(deftest test-deserialize-undefined
  (testing "should #(#xf7) deserialize to :undefined"
    (ok (eql (deserialize (make-test-stream #(#xf7))) +undefined+))))

(deftest test-deserialize-simple-16
  (testing "should #(#xf0) deserialize to #<simple 16>"
    (let ((result (deserialize (make-test-stream #(#xf0)))))
      (ok (and (typep result 'simple-value)
	       (eql (simple-value-value result) 16))))))

(deftest test-deserialize-simple-255
  (testing "should #(#xf8 #xff) deserialize to #<simple 255>"
    (let ((result (deserialize (make-test-stream #(#xf8 #xff)))))
      (ok (and (typep result 'simple-value)
	       (eql (simple-value-value result) 255))))))

(deftest test-deserialize-time-rfc3339
  (testing "should #(#xc0 #x74 #x32 #x30 #x31 #x33 #x2d #x30 #x33 #x2d
#x32 #x31 #x54 #x32 #x30 #x3a #x30 #x34 #x3a #x30 #x30 #x5a) deserialize to timestamp
represented by \"2013-03-21T20:04:00Z\""
    (let ((result (deserialize (make-test-stream #(#xc0 #x74 #x32 #x30 #x31 #x33 #x2d #x30 #x33 #x2d
						   #x32 #x31 #x54 #x32 #x30 #x3a #x30 #x34 #x3a #x30 #x30 #x5a)))))
      (ok (local-time:timestamp= result (local-time:parse-rfc3339-timestring "2013-03-21T20:04:00Z"))))))

(deftest test-deserialize-time-epoch
  (testing "should #(#xc1 #x1a #x51 #x4b #x67 #xb0) deserialize to 1363896240 seconds
after the UNIX epoch"
    (let ((result (deserialize (make-test-stream #(#xc1 #x1a #x51 #x4b #x67 #xb0)))))
      (ok (local-time:timestamp= result (local-time:unix-to-timestamp 1363896240))))))

(deftest test-deserialize-base64url
  (testing "should #(#xd5 #x44 #x01 #x02 #x03 #x04) deserialize to byte array
#(#x01 #x02 #x03 #x04), while having a hint for text serialization to be done as base64url"
    (let ((result (deserialize (make-test-stream #(#xd5 #x44 #x01 #x02 #x03 #x04)))))
      (ok (typep result 'base64url-data))
      (ok (equalp (base64url-data-content result)
		  #(#x01 #x02 #x03 #x04))))))

(deftest test-deserialize-base64
  (testing "should #(#xd6 #x44 #x01 #x02 #x03 #x04) deserialize to byte array
#(#x01 #x02 #x03 #x04), while having a hint for text serialization to be done as base64"
    (let ((result (deserialize (make-test-stream #(#xd6 #x44 #x01 #x02 #x03 #x04)))))
      (ok (typep result 'base64-data))
      (ok (equalp (base64-data-content result)
		  #(#x01 #x02 #x03 #x04))))))

(deftest test-deserialize-base16
  (testing "should #(#xd7 #x44 #x01 #x02 #x03 #x04) deserialize to byte array
#(#x01 #x02 #x03 #x04), while having a hint for text serialization to be done as base16"
    (let ((result (deserialize (make-test-stream #(#xd7 #x44 #x01 #x02 #x03 #x04)))))
      (ok (typep result 'base16-data))
      (ok (equalp (base16-data-content result)
		  #(#x01 #x02 #x03 #x04))))))

(deftest test-deserialize-encoded-cbor
  (testing "should #(#xd8 #x18 #x45 #x64 #x49 #x45 #x54 #x46) deserialize to byte array
#(#x64 #x49 #x45 #x54 #x46), which represents nested CBOR data"
    (let ((result (deserialize (make-test-stream #(#xd8 #x18 #x45 #x64 #x49 #x45 #x54 #x46)))))
      (ok (typep result 'nested-cbor))
      (ok (equalp (nested-cbor-data result) #(#x64 #x49 #x45 #x54 #x46))))))

(deftest test-deserialize-empty-bytevector
  (testing "should #(#x40) deserialize to an empty byte array"
    (let ((result (deserialize (make-test-stream #(#x40)))))
      (ok (equalp result #())))))

(deftest test-deserialize-populated-bytevector
  (testing "should #(#x44 #x01 #x02 #x03 #x04) deserialize to byte array #(#x01 #x02 #x03 #x04)"
    (let ((result (deserialize (make-test-stream #(#x44 #x01 #x02 #x03 #x04)))))
      (ok (equalp result #(#x01 #x02 #x03 #x04))))))

(deftest test-deserialize-empty-string
  (testing "should #(#x60) deserialize to an empty string"
    (let ((result (deserialize (make-test-stream #(#x60)))))
      (ok (string-equal result "")))))

(deftest test-deserialize-string-a
  (testing "should #(#x61 #x61) deserialize to the string \"a\""
    (let ((result (deserialize (make-test-stream #(#x61 #x61)))))
      (ok (string-equal result "a")))))

(deftest test-deserialize-string-IETF
  (testing "should #(#x64 #x49 #x45 #x54 #x46) deserialize to the string \"IETF\""
    (let ((result (deserialize (make-test-stream #(#x64 #x49 #x45 #x54 #x46)))))
      (ok (string-equal result "IETF")))))

(deftest test-deserialize-string-escapes
  (testing "should #(#x62 #x22 #x5c) deserialize to the string \"\\\"\\\\\""
    (let ((result (deserialize (make-test-stream #(#x62 #x22 #x5c)))))
      (ok (string-equal result "\"\\")))))

(deftest test-deserialize-string-unicode
  (testing "should #(#x62 #xc3 #xbc) deserialize to the string \"ü\""
    (let ((result (deserialize (make-test-stream #(#x62 #xc3 #xbc)))))
      (ok (string-equal result "ü")))))

(deftest test-deserialize-string-ideograph
  (testing "should #(#x63 #xe6 #xb0 #xb4) deserialize to the string \"水\""
    (let ((result (deserialize (make-test-stream #(#x63 #xe6 #xb0 #xb4)))))
      (ok (string-equal result "水")))))

(deftest test-deserialize-string-surrogate
  (testing "should #(#x64 #xf0 #x90 #x85 #x91) deserialize to the string \"𐅑\" from surrogate pairs"
    (let ((result (deserialize (make-test-stream #(#x64 #xf0 #x90 #x85 #x91)))))
      (ok (string-equal result "𐅑")))))

(deftest test-deserialize-empty-array
  (testing "should #(#x80) deserialize to an empty array"
    (let ((result (deserialize (make-test-stream #(#x80)))))
      (ok (equalp result #())))))

(deftest test-deserialize-array-simple-populated
  (testing "should #(x83 #x01 #x02 #x03) deserialize to the array #(1 2 3)"
    (let ((result (deserialize (make-test-stream #(#x83 #x01 #x02 #x03)))))
      (ok (equalp result #(1 2 3))))))

(deftest test-deserialize-array-nested
  (testing "should #(#x83 #x01 #x82 #x02 #x03 #x82 #x04 #x05) deserialize to the array #(1 #(2 3) #(4 5))"
    (let ((result (deserialize (make-test-stream #(#x83 #x01 #x82 #x02 #x03 #x82 #x04 #x05)))))
      (ok (equalp result #(1 #(2 3) #(4 5)))))))

(deftest test-deserialize-array-large
    (testing "should #(#x98 #x19 #x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08 #x09 #x0a #x0b #x0c #x0d
#x0e #x0f #x10 #x11 #x12 #x13 #x14 #x15 #x16 #x17 #x18 #x18 #x18 #x19) deserialize to the array
#(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25)"
	     (let ((result (deserialize (make-test-stream #(#x98 #x19 #x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08 #x09 #x0a
							    #x0b #x0c #x0d #x0e #x0f #x10 #x11 #x12 #x13 #x14 #x15 #x16
							    #x17 #x18 #x18 #x18 #x19)))))
	       (ok (equalp result #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25))))))

(deftest test-deserialize-empty-map
  (testing "should #(#xa0) deserialize to an empty map"
    (let ((result (deserialize (make-test-stream #(#xa0)))))
      (ok (hash-table-p result))
      (ok (zerop (hash-table-count result))))))

(deftest test-deserialize-map-simple
  (testing "should #(#xa2 #x01 #x02 #x03 #x04) deserialize to map {1: 2, 3: 4}"
    (let ((result (deserialize (make-test-stream #(#xa2 #x01 #x02 #x03 #x04))))
	  (expected (make-hash-table :test #'equal)))
      (setf (gethash 1 expected) 2)
      (setf (gethash 3 expected) 4)
      (ok (equalp result expected)))))

(deftest test-deserialize-map-complex
  (testing "should #(#xa2 #x61 #6x1 #x01 #x61 #x62 #x82 #x02 #x03)
deserialize to map {\"a\": 1, \"b\": #(2 3)}"
    (let ((result (deserialize (make-test-stream #(#xa2 #x61 #x61 #x01 #x61 #x62 #x82 #x02 #x03))))
	  (expected (make-hash-table :test #'equal)))
      (setf (gethash "a" expected) 1)
      (setf (gethash "b" expected) #(2 3))
      (ok (equalp result expected)))))

(deftest test-deserialize-array-map
  (testing "should #(#x82 #x61 #x61 #xa1 #x61 #x62 #x61 #x63) deserialize to an array with map
#(\"a\" {\"b\": \"c\"})"
    (let* ((result (deserialize (make-test-stream #(#x82 #x61 #x61 #xa1 #x61 #x62 #x61 #x63))))
	   (nested-map (make-hash-table :test #'equal))
	   (expected `#("a" ,nested-map)))
      (setf (gethash "b" nested-map) "c")
      (ok (equalp result expected)))))

(deftest test-deserialize-map-large
  (testing "should #(#xa5 #x61 #x61 #x61 #x41 #x61 #x62 #x61 #x42 #x61 #x63 #x61 #x43 #x61 #x64 #x61 #x44 #x61 #x65 #x61 #x45)
deserialize to {\"a\": \"A\", \"b\": \"B\", \"c\": \"C\", \"d\": \"D\", \"e\": \"E\"}"
    (let ((result (deserialize (make-test-stream #(#xa5 #x61 #x61 #x61 #x41
						   #x61 #x62 #x61 #x42 #x61
						   #x63 #x61 #x43 #x61 #x64
						   #x61 #x44 #x61 #x65 #x61 #x45))))
	  (expected (make-hash-table :test #'equal)))
      (setf (gethash "a" expected) "A")
      (setf (gethash "b" expected) "B")
      (setf (gethash "c" expected) "C")
      (setf (gethash "d" expected) "D")
      (setf (gethash "e" expected) "E")
      (ok (equalp result expected)))))
