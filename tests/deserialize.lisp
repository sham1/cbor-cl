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
