#|                                           -*- mode: lisp; coding: utf-8 -*-
  Deterministic Arts -- UUID support
  Copyright (c) 2016 Dirk Esser

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

(in-package "DARTS.LIB.UUID")

(defstruct (uuid (:copier nil) (:constructor %make-uuid (low-word high-word))
                 (:predicate uuidp))
  (low-word (error "missing low word") :type (unsigned-byte 64) :read-only t)
  (high-word (error "missing high word") :type (unsigned-byte 64) :read-only t))

(defun uuid= (u1 u2)
  (and (eql (uuid-low-word u1) (uuid-low-word u2))
       (eql (uuid-high-word u1) (uuid-high-word u2))))

(defun uuid< (u1 u2)
  (let ((h1 (uuid-high-word u1)) 
        (h2 (uuid-high-word u2)))
    (cond
      ((< h1 h2) t)
      ((< h2 h1) nil)
      (t (< (uuid-low-word u1)
            (uuid-low-word u2))))))

(defun uuid-hash (object)
  (logxor (sxhash (uuid-high-word object))
	  (sxhash (uuid-low-word object))))

(declaim (inline uuid/= uuid<= uuid>= uuid>))

(defun uuid/= (u1 u2)
  (not (uuid= u1 u2)))

(defun uuid> (u1 u2)
  (uuid< u2 u1))

(defun uuid>= (u1 u2)
  (not (uuid< u1 u2)))

(defun uuid<= (u1 u2)
  (not (uuid< u2 u1)))

(defun print-uuid (object &key (stream *standard-output*) braces downcase)
  (let ((high (uuid-high-word object))
        (low (uuid-low-word object))
        (digits (if downcase "0123456789abcdef" "0123456789ABCDEF")))
    (labels
        ((dash () (write-char #\- stream))
         (digits (number count)
           (loop
             :for k :downfrom (1- count) :to 0
             :do (write-char (char digits (ldb (byte 4 (* 4 k)) number)) stream))))
      (when braces (write-char #\{ stream))
      (digits (ash high -32) 8)
      (dash)
      (digits (ash high -16) 4)
      (dash)
      (digits high 4)
      (dash)
      (digits (ash low -48) 4)
      (dash)
      (digits low 12)
      (when braces (write-char #\} stream))
      object)))

(defmethod print-object ((object uuid) stream)
  (if (not *print-escape*)
      (print-uuid object :stream stream :braces t)
      (print-unreadable-object (object stream :type t :identity nil)
        (print-uuid object :stream stream))))

(defun uuid-bytes (object)
  (let ((array (make-array 16 :element-type '(unsigned-byte 8))))
    (flet ((emplace (number pointer)
             (loop
               :for k :downfrom 56 :to 0 :by 8 
               :for p :upfrom pointer
               :do (setf (aref array p) (ldb (byte 8 k) number)))))
      (emplace (uuid-high-word object) 0)
      (emplace (uuid-low-word object) 8)
      array)))

(defun uuid-number (object)
  (logior (ash (uuid-high-word object) 64) 
          (uuid-low-word object)))

(defun uuid-version (object)
  (logand #x0f (ash (uuid-high-word object) -12)))

(defun uuid-variant (object)
  (let ((byte (ldb (byte 8 56) (uuid-low-word object))))
    (cond
      ((zerop (logand byte #b10000000)) :ncs)
      ((eql (logand byte #b11000000) #b10000000) :leach-salz)
      ((eql (logand byte #b11100000) #b11000000) :microsoft)
      (t :reserved))))

(defun uuid-timestamp (object)
  (and (eql (uuid-version object) 1)
       (logior (ash (logand (uuid-high-word object) #x0FFF) 48)
               (ash (logand (ash (uuid-high-word object) -16) #xFFFF) 32)
               (ash (uuid-high-word object) -32))))

(defun uuid-clock-sequence (object)
  (and (eql (uuid-version object) 1)
       (ash (logand (uuid-low-word object) #x3FFF000000000000) -48)))

(defun uuid-node (object)
  (and (eql (uuid-version object) 1)
       (logand (uuid-low-word object) #xFFFFFFFFFFFF)))


(defun uuid-string (object &key braces downcase)
  (with-output-to-string (stream)
    (print-uuid object 
                :stream stream 
                :braces braces 
                :downcase downcase)))

(defun uuid-string-p (object
                      &key lenient braces)
  (and (stringp object)
       (let ((start 0) (end (length object)))
         (when braces
           (unless (> (length object) 2) (return-from uuid-string-p nil))
           (unless (eql (char object 0) #\{) (return-from uuid-string-p nil))
           (unless (eql (char object (1- end)) #\}) (return-from uuid-string-p nil))
           (incf start) (decf end))
         (if lenient
             (scan "^([0-9a-fA-F]{1,8})-([0-9a-fA-F]{1,4})-([0-9a-fA-F]{1,4})-([0-9a-fA-F]{1,4})-([0-9a-fA-F]{1,12})$"
                   object :start start :end end)
             (scan "^([0-9a-fA-F]{8})-([0-9a-fA-F]{4})-([0-9a-fA-F]{4})-([0-9a-fA-F]{4})-([0-9a-fA-F]{12})$"
                   object :start start :end end)))
       t))

(defun parse-uuid (value &key (start 0) end)
  (let* ((string (string value))
         (end (or end (length string))))
    (when (and (< start end) (eql (char string start) #\{))
      (incf start)
      (unless (and (< start end) (eql (char string (1- end)) #\}))
        (return-from parse-uuid nil))
      (decf end))
    (multiple-value-bind (match-start match-end group-starts group-ends) (scan "^([0-9a-fA-F]{1,8})-([0-9a-fA-F]{1,4})-([0-9a-fA-F]{1,4})-([0-9a-fA-F]{1,4})-([0-9a-fA-F]{1,12})$" 
                                                                               string :start start :end end)
      (declare (ignore match-end))
      (and match-start
           (flet ((parse-group (k)
                    (parse-integer string :start (aref group-starts k) :end (aref group-ends k) :radix 16)))
             (%make-uuid 
              (logior (ash (parse-group 3) 48)
                      (parse-group 4))
              (logior (ash (parse-group 0) 32)
                      (ash (parse-group 1) 16)
                      (parse-group 2))))))))

(defun uuid (object)
  (labels
      ((bad-value ()
         (error 'simple-type-error
                :datum object :expected-type 'uuid
                :format-control "~S is not a well-formed UUID value" 
                :format-arguments (list object))))
    (typecase object
      (uuid object)
      (string (or (parse-uuid object) (bad-value)))
      (symbol (or (parse-uuid (symbol-name object)) (bad-value)))
      ((unsigned-byte 128) (%make-uuid (logand object #xFFFFFFFFFFFFFFFF) (ash object -64)))
      ((array (unsigned-byte 8) (16)) 
       (let ((high 0) (low 0))
         (loop
           :for k :upfrom 0
           :for j :downfrom 56 :to 0 :by 8
           :do (setf (ldb (byte 8 j) high) (aref object k)))
         (loop
           :for k :upfrom 8 
           :for j :downfrom 56 :to 0 :by 8
           :do (setf (ldb (byte 8 j) low) (aref object k)))
         (%make-uuid low high)))
      (t (bad-value)))))

(defmethod make-load-form ((object uuid) &optional environment)
  (declare (ignore environment))
  `(%make-uuid ,(uuid-low-word object) ,(uuid-high-word object)))

#-(and)
(define-compiler-macro uuid (&whole form value)
  (cond
    ((uuidp value) `(quote ,value))
    ((stringp value)
     (let ((uuid (parse-uuid value)))
       (if uuid
           `(quote ,uuid)
           form)))
    (t form)))

(defun set-version (buffer version)
  (declare (type (array (unsigned-byte 8) (*)) buffer)
           (type (unsigned-byte 8) version))
  (setf (aref buffer 6) (logior (logand (aref buffer 6) #x0f) version))
  (setf (aref buffer 8) (logior (logand (aref buffer 8) #x3f) #x80))
  buffer)


(defun random-uuid (&key (generator nil have-generator) (random-state *random-state* have-random-state))
  "Construct a new random UUID. If `generator` is supplied, it must be a function
   callable with a single argument (the number of random bytes to generate), which
   answers an array of \"random\" `(unsigned-byte 8)` numbers. If no generator
   function is provided, the UUID is generated using Lisp's built-in `random`
   function, passing the given `random-state` along, which defaults to the value
   of the global `*random-state*` variable."
  (declare (notinline uuid))
  (when (and have-generator have-random-state)
    (error "cannot use both, a ~S function and ~S"
           :generator :random-state))
  (let ((buffer (make-array 16 :element-type '(unsigned-byte 8))))
    (if have-generator
        (let ((bytes (funcall generator 16)))
          (loop
            :for k :upfrom 0 :below 16
            :do (setf (aref buffer k) (aref bytes k))))
        (loop
          :for k :upfrom 0 :below 16
          :do (setf (aref buffer k) (random 256 random-state))))
    (uuid (set-version buffer #x40))))


(defun uuid-for-name (string 
                      &key (start 0) end (digest :md5) (namespace nil))
  (let* ((string (string string))
         (end (or end (length string)))
         (bytes (string-to-utf-8-bytes (subseq string start end) :null-terminate nil))
         (buffer (set-version (let ((digest (make-digest digest)))
                                (when namespace
                                  (let ((bytes (uuid-bytes (uuid namespace))))
                                    (update-digest digest bytes)))
                                (update-digest digest bytes)
                                (produce-digest digest))
                              (ecase digest
                                ((:md5) #x30)
                                ((:sha1) #x50)))))
    (let ((w1 0) (w2 0))
      (loop
        :for rp :upfrom 0 :below 8
        :for wp :downfrom 56 :to 0 :by 8
        :do (setf (ldb (byte 8 wp) w1) (aref buffer rp)))
      (loop
        :for rp :upfrom 8 :below 16
        :for wp :downfrom 56 :to 0 :by 8
        :do (setf (ldb (byte 8 wp) w2) (aref buffer rp)))
      (%make-uuid w2 w1))))

