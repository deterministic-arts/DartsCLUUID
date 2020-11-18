#|                                           -*- mode: lisp; coding: utf-8 -*-
  Deterministic Arts -- UUID support
  Copyright (c) 2020 Dirk Esser

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  THE SOFTWARE.
|#

(in-package #:darts.lib.uuid-test)

(defsuite uuid-suite)
(in-suite uuid-suite)

(deftest uuid-number-round-trips ()
  (let ((value (uuid "871044C9-1DE3-437B-B24D-85084B5974D3")))
    (is (uuid= value (uuid (uuid-number value))))))

(deftest uuid-string-round-trips ()
  (let ((value (uuid "871044C9-1DE3-437B-B24D-85084B5974D3")))
    (is (uuid= value (uuid (uuid-string value))))))

(deftest uuid-bytes-round-trips ()
  (let ((value (uuid "871044C9-1DE3-437B-B24D-85084B5974D3")))
    (is (uuid= value (uuid (uuid-bytes value))))))

(deftest uuid-ordering ()
  (let* ((strings (sort (list "251018C2-123D-45BD-9362-BBC0FBA08232" "41FA4BF9-7011-4ACE-9607-D9A721A74894" "70D12396-4DD9-415B-A56A-054BE3269523") #'string-lessp)))
    (loop
      for outer-string in strings
      as outer = (uuid outer-string)
      do (loop
           for inner-string in strings
           as inner = (uuid inner-string)
           do (macrolet
                  ((same-result (string-op uuid-op)
                     `(is (eq (and (,string-op outer-string inner-string) t)
                              (and (,uuid-op outer inner) t)))))
                (same-result string-equal uuid=)
                (same-result string-not-equal uuid/=)
                (same-result string-lessp uuid<)
                (same-result string-greaterp uuid>)
                (same-result string-not-lessp uuid>=)
                (same-result string-not-greaterp uuid<=))))))

(deftest uuid-hashing ()
  (let* ((strings (list "251018C2-123D-45BD-9362-BBC0FBA08232" "41FA4BF9-7011-4ACE-9607-D9A721A74894" "70D12396-4DD9-415B-A56A-054BE3269523")))
    (loop
      for outer-string in strings
      as outer = (uuid outer-string)
      do (loop
           for inner-string in strings
           as inner = (uuid inner-string)
           do (if (string-not-equal outer-string inner-string)
                  (progn
                    (is (not (uuid= outer inner)))
                    (is (uuid/= outer inner))
                    (is (or (uuid< outer inner) (uuid> outer inner)))
                    (is (or (uuid<= outer inner) (uuid>= outer inner))))
                  (progn
                    (is (uuid= outer inner))
                    (is (not (uuid/= outer inner)))
                    (is (eql (uuid-hash outer) (uuid-hash inner)))
                    (is (not (or (uuid< outer inner) (uuid> outer inner))))
                    (is (and (uuid<= outer inner) (uuid>= outer inner)))))))))
