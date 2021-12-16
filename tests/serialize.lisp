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
