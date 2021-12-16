(defpackage cbor-cl/tests/serialize
  (:use
   :cl
   :cbor-cl
   :flexi-streams
   :rove))
(in-package :cbor-cl/tests/serialize)

;;;; This tests the serialization of proper Lisp data structures into CBOR data.
;;;; All of the test-data is taken from RFC 8949.

;; NOTE: To run this test file, execute `(asdf:test-system :cbor-cl)' in your Lisp.

(deftest test-serialize-0
  (testing "should 0 serialize to #(#x00)"
    (let ((stream (make-in-memory-output-stream)))
      (serialize stream 0)
      (ok (equalp #(#x00) (get-output-stream-sequence stream))))))

(deftest test-serialize-1
  (testing "should 1 serialize to #(#x01)"
    (let ((stream (make-in-memory-output-stream)))
      (serialize stream 1)
      (ok (equalp #(#x01) (get-output-stream-sequence stream))))))

(deftest test-serialize-10
  (testing "should 10 serialize to #(#x0a)"
    (let ((stream (make-in-memory-output-stream)))
      (serialize stream 10)
      (ok (equalp #(#x0a) (get-output-stream-sequence stream))))))

(deftest test-serialize-23
  (testing "should 23 serialize to #(#x17)"
    (let ((stream (make-in-memory-output-stream)))
      (serialize stream 23)
      (ok (equalp #(#x17) (get-output-stream-sequence stream))))))

(deftest test-serialize-24
  (testing "should 24 serialize to #(#x18 #x18)"
    (let ((stream (make-in-memory-output-stream)))
      (serialize stream 24)
      (ok (equalp #(#x18 #x18) (get-output-stream-sequence stream))))))

(deftest test-serialize-25
  (testing "should 25 serialize to #(#x18 #x19)"
    (let ((stream (make-in-memory-output-stream)))
      (serialize stream 25)
      (ok (equalp #(#x18 #x19) (get-output-stream-sequence stream))))))

(deftest test-serialize-100
  (testing "should 100 serialize to #(#x18 #x64)"
    (let ((stream (make-in-memory-output-stream)))
      (serialize stream 100)
      (ok (equalp #(#x18 #x64) (get-output-stream-sequence stream))))))

(deftest test-serialize-1000
  (testing "should 1000 serialize to #(#x19 #x03 #xe8)"
    (let ((stream (make-in-memory-output-stream)))
      (serialize stream 1000)
      (ok (equalp #(#x19 #x03 #xe8) (get-output-stream-sequence stream))))))

(deftest test-serialize-1000000
  (testing "should 1000000 serialize to #(#x1a #x00 #x0f #x42 #x40)"
    (let ((stream (make-in-memory-output-stream)))
      (serialize stream 1000000)
      (ok (equalp #(#x1a #x00 #x0f #x42 #x40) (get-output-stream-sequence stream))))))

(deftest test-serialize-1000000000000
  (testing "should 1000000000000 serialize to #(#x1b #x00 #x00 #x00 #xe8 #xd4 #xa5 #x10 #x00)"
    (let ((stream (make-in-memory-output-stream)))
      (serialize stream 1000000000000)
      (ok (equalp #(#x1b #x00 #x00 #x00 #xe8 #xd4 #xa5 #x10 #x00) (get-output-stream-sequence stream))))))

(deftest test-serialize-18446744073709551615
  (testing "should 18446744073709551615 serialize to #(#x1b #xff #xff #xff #xff #xff #xff #xff #xff)"
    (let ((stream (make-in-memory-output-stream)))
      (serialize stream 18446744073709551615)
      (ok (equalp #(#x1b #xff #xff #xff #xff #xff #xff #xff #xff) (get-output-stream-sequence stream))))))

(deftest test-serialize-18446744073709551616
  (testing "should 18446744073709551616 serialize to #(#xc2 #x49 #x01 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00)"
    (let ((stream (make-in-memory-output-stream)))
      (serialize stream 18446744073709551616)
      (ok (equalp #(#xc2 #x49 #x01 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00) (get-output-stream-sequence stream))))))

(deftest test-serialize--18446744073709551616
  (testing "should -18446744073709551616 serialize to #(#x3b #xff #xff #xff #xff #xff #xff #xff #xff)"
    (let ((stream (make-in-memory-output-stream)))
      (serialize stream -18446744073709551616)
      (ok (equalp #(#x3b #xff #xff #xff #xff #xff #xff #xff #xff) (get-output-stream-sequence stream))))))

(deftest test-serialize--18446744073709551617
  (testing "should -18446744073709551617 serialize to #(#xc3 #x49 #x01 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00)"
    (let ((stream (make-in-memory-output-stream)))
      (serialize stream -18446744073709551617)
      (ok (equalp #(#xc3 #x49 #x01 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00) (get-output-stream-sequence stream))))))

(deftest test-serialize--1
  (testing "should -1 serialize to #(#x20)"
    (let ((stream (make-in-memory-output-stream)))
      (serialize stream -1)
      (ok (equalp #(#x20) (get-output-stream-sequence stream))))))

(deftest test-serialize--10
  (testing "should -10 serialize to #(#x29)"
    (let ((stream (make-in-memory-output-stream)))
      (serialize stream -10)
      (ok (equalp #(#x29) (get-output-stream-sequence stream))))))

(deftest test-serialize--100
  (testing "should -100 serialize to #(#x38 #x63)"
    (let ((stream (make-in-memory-output-stream)))
      (serialize stream -100)
      (ok (equalp #(#x38 #x63) (get-output-stream-sequence stream))))))

(deftest test-serialize--1000
  (testing "should -1000 serialize to #(#x39 #x03 #xe7)"
    (let ((stream (make-in-memory-output-stream)))
      (serialize stream -1000)
      (ok (equalp #(#x39 #x03 #xe7) (get-output-stream-sequence stream))))))

;; TODO: Add support for floating-point

(deftest test-serialize-false
  (testing "should nil serialize to #(#xf4)"
    (let ((stream (make-in-memory-output-stream)))
      (serialize stream +false+)
      (ok (equalp #(#xf4) (get-output-stream-sequence stream))))))

(deftest test-serialize-true
  (testing "should t serialize to #(#xf5)"
    (let ((stream (make-in-memory-output-stream)))
      (serialize stream +true+)
      (ok (equalp #(#xf5) (get-output-stream-sequence stream))))))

(deftest test-serialize-null
  (testing "should :null serialize to #(#xf6)"
    (let ((stream (make-in-memory-output-stream)))
      (serialize stream +null+)
      (ok (equalp #(#xf6) (get-output-stream-sequence stream))))))

(deftest test-serialize-undefined
  (testing "should nil serialize to #(#xf7)"
    (let ((stream (make-in-memory-output-stream)))
      (serialize stream +undefined+)
      (ok (equalp #(#xf7) (get-output-stream-sequence stream))))))

(deftest test-serialize-simple-16
  (testing "should #<simple 16> serialize to #(#xf0)"
    (let ((stream (make-in-memory-output-stream)))
      (serialize stream (make-instance 'simple-value :value 16))
      (ok (equalp #(#xf0) (get-output-stream-sequence stream))))))

(deftest test-serialize-simple-255
  (testing "should #<simple 255> serialize to #(#xf8 #xff)"
    (let ((stream (make-in-memory-output-stream)))
      (serialize stream (make-instance 'simple-value :value 255))
      (ok (equalp #(#xf8 #xff) (get-output-stream-sequence stream))))))

;; TODO: Add serialization for common tags

(deftest test-serialize-empty-bytevector
  (testing "should an empty byte array serialize to #(#x40)"
    (let ((stream (make-in-memory-output-stream)))
      (serialize stream (make-array 0 :element-type '(unsigned-byte 8)))
      (ok (equalp #(#x40) (get-output-stream-sequence stream))))))

(deftest test-serialize-populated-bytevector
  (testing "should byte array #(#x01 #x02 #x03 #x04) serialize to #(#x44 #x01 #x02 #x03 #x04)"
    (let ((stream (make-in-memory-output-stream)))
      (serialize stream (make-array 4 :element-type '(unsigned-byte 8) :initial-contents #(#x01 #x02 #x03 #x04)))
      (ok (equalp #(#x44 #x01 #x02 #x03 #x04) (get-output-stream-sequence stream))))))

(deftest test-serialize-empty-string
  (testing "should an empty string serialize to #(#x60)"
    (let ((stream (make-in-memory-output-stream)))
      (serialize stream "")
      (ok (equalp #(#x60) (get-output-stream-sequence stream))))))

(deftest test-serialize-string-a
  (testing "should a string \"a\" serialize to #(#x61 #x61)"
    (let ((stream (make-in-memory-output-stream)))
      (serialize stream "a")
      (ok (equalp #(#x61 #x61) (get-output-stream-sequence stream))))))

(deftest test-serialize-string-IETF
  (testing "should a string \"IETF\" serialize to #(#x64 #x49 #x45 #x54 #x46)"
    (let ((stream (make-in-memory-output-stream)))
      (serialize stream "IETF")
      (ok (equalp #(#x64 #x49 #x45 #x54 #x46) (get-output-stream-sequence stream))))))

(deftest test-serialize-string-escapes
  (testing "should a string \"\\\"\\\\\" serialize to #(#x62 #x22 #x5c)"
    (let ((stream (make-in-memory-output-stream)))
      (serialize stream "\"\\")
      (ok (equalp #(#x62 #x22 #x5c) (get-output-stream-sequence stream))))))

(deftest test-serialize-string-unicode
  (testing "should a string \"ü\" serialize to #(#x62 #xc3 #xbc)"
    (let ((stream (make-in-memory-output-stream)))
      (serialize stream "ü")
      (ok (equalp #(#x62 #xc3 #xbc) (get-output-stream-sequence stream))))))

(deftest test-serialize-string-ideograph
  (testing "should a string \"水\" serialize to #(#x63 #xe6 #xb0 #xb4)"
    (let ((stream (make-in-memory-output-stream)))
      (serialize stream "水")
      (ok (equalp #(#x63 #xe6 #xb0 #xb4) (get-output-stream-sequence stream))))))

(deftest test-serialize-string-surrogate
  (testing "should a string \"𐅑\" serialize to #(#x64 #xf0 #x90 #x85 #x91)"
    (let ((stream (make-in-memory-output-stream)))
      (serialize stream "𐅑")
      (ok (equalp #(#x64 #xf0 #x90 #x85 #x91) (get-output-stream-sequence stream))))))

(deftest test-serialize-empty-array
  (testing "should an empty array serialize to #(#x80)"
    (let ((stream (make-in-memory-output-stream)))
      (serialize stream #())
      (ok (equalp #(#x80) (get-output-stream-sequence stream))))))

(deftest test-serialize-array-simple-populated
  (testing "should an array #(1 2 3) serialize to #(#x83 #x01 #x02 #x03)"
    (let ((stream (make-in-memory-output-stream)))
      (serialize stream #(1 2 3))
      (ok (equalp #(#x83 #x01 #x02 #x03) (get-output-stream-sequence stream))))))

(deftest test-serialize-array-nested
  (testing "should an array #(1 #(2 3) #(4 5)) serialize to #(#x83 #x01 #x82 #x02 #x03 #x82 #x04 #x05)"
    (let ((stream (make-in-memory-output-stream)))
      (serialize stream #(1 #(2 3) #(4 5)))
      (ok (equalp #(#x83 #x01 #x82 #x02 #x03 #x82 #x04 #x05) (get-output-stream-sequence stream))))))

(deftest test-serialize-array-large
  (testing "should an array #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25)
serialize to #(#x98 #x19 #x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08 #x09 #x0a #x0b #x0c #x0d
#x0e #x0f #x10 #x11 #x12 #x13 #x14 #x15 #x16 #x17 #x18 #x18 #x18 #x19)"
    (let ((stream (make-in-memory-output-stream)))
      (serialize stream #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25))
      (ok (equalp #(#x98 #x19 #x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08 #x09 #x0a #x0b #x0c #x0d
		    #x0e #x0f #x10 #x11 #x12 #x13 #x14 #x15 #x16 #x17 #x18 #x18 #x18 #x19)
		  (get-output-stream-sequence stream))))))

;;; With maps, we need to rely on `deserialize' working properly,
;;; since the serialized fields could be in any order within the
;;; stream.
(deftest test-serialize-empty-map
  (testing "should an empty map serialize to #(#a0)"
    (let ((stream (make-in-memory-output-stream)))
      (serialize stream (make-hash-table))
      (ok (equalp #(#xa0) (get-output-stream-sequence stream))))))

(deftest test-serialize-map-simple
  (testing "should a map {1: 2, 3: 4} serialize to #(#a2 #x01 #x02 #x03 #x04)"
    (let ((stream (make-in-memory-output-stream))
	  (map (make-hash-table :test 'equal)))
      (setf (gethash 1 map) 2)
      (setf (gethash 3 map) 4)

      (serialize stream map)
      (ok (equalp map (deserialize (make-in-memory-input-stream (get-output-stream-sequence stream))))))))

(deftest test-serialize-map-complex
  (testing "should a map {\"a\": 1, \"b\": #(2 3)} serialize to #(#xa2 #x61 #6x1 #x01 #x61 #x62 #x82 #x02 #x03)"
    (let ((stream (make-in-memory-output-stream))
	  (map (make-hash-table :test 'equal)))
      (setf (gethash "a" map) 1)
      (setf (gethash "b" map) #(2 3))

      (serialize stream map)
      (ok (equalp map (deserialize (make-in-memory-input-stream (get-output-stream-sequence stream))))))))

(deftest test-serialize-array-map
  (testing "should an array with map #(\"a\" {\"b\": \"c\"}) serialize to #(#x82 #x61 #x61 #xa1 #x61 #x62 #x61 #x63)"
    (let ((stream (make-in-memory-output-stream))
	  (map (make-hash-table :test 'equal)))
      (setf (gethash "b" map) "c")

      (serialize stream `#(1 ,map))
      (ok (equalp `#(1 ,map) (deserialize (make-in-memory-input-stream (get-output-stream-sequence stream))))))))

(deftest test-serialize-map-large
  (testing "should a map {\"a\": \"A\", \"b\": \"B\", \"c\": \"C\", \"d\": \"D\", \"e\": \"E\"}
 serialize to #(#xa5 #x61 #x61 #x61 #x41 #x61 #x62 #x61 #x42 #x61 #x63 #x61 #x43 #x61 #x64 #x61 #x44 #x61 #x65 #x61 #x45)"
    (let ((stream (make-in-memory-output-stream))
	  (map (make-hash-table :test 'equal)))

      (setf (gethash "a" map) "A")
      (setf (gethash "b" map) "B")
      (setf (gethash "c" map) "C")
      (setf (gethash "d" map) "D")
      (setf (gethash "e" map) "E")

      (serialize stream map)
      (ok (equalp map (deserialize (make-in-memory-input-stream (get-output-stream-sequence stream))))))))
