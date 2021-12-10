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

(deftest test-deserialize-base16
  (testing "should #(#xd7 #x44 #x01 #x02 #x03 #x04) deserialize to byte array
#(#x01 #x02 #x03 #x04), while having a hint for text serialization to be done as base16"
    (let ((result (deserialize (make-test-stream #(#xd7 #x44 #x01 #x02 #x03 #x04)))))
      (ok (equalp result #(#x01 #x02 #x03 #x04))))))

(deftest test-deserialize-encoded-cbor
  (testing "should #(#xd8 #x18 #x45 #x64 #x49 #x45 #x54 #x46) deserialize to byte array
#(#x64 #x49 #x45 #x54 #x46), which represents nested CBOR data"
    (let ((result (deserialize (make-test-stream #(#xd8 #x18 #x45 #x64 #x49 #x45 #x54 #x46)))))
      (ok (equalp result #(#x64 #x49 #x45 #x54 #x46))))))
