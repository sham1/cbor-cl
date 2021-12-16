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
