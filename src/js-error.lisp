;;;; The MIT License (MIT)

;;;; Copyright (c) 2015 Huang Xuxing

;;;; Permission is hereby granted, free of charge, to any person obtaining
;;;; a copy of this software and associated documentation files
;;;; (the "Software"), to deal in the Software without restriction,
;;;; including without limitation the rights to use, copy, modify, merge,
;;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;;; and to permit persons to whom the Software is furnished to do so,
;;;; subject to the following conditions:

;;;; The above copyright notice and this permission notice shall be included
;;;; in all copies or substantial portions of the Software.

;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;;;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;; OTHER DEALINGS IN THE SOFTWARE.

(in-package :lesp-builtin)

;;;; Error related definitions and methods.
(defclass -error-proto (-object-proto)
  ((-prototype :initform (find-class '-object-proto))
   (-error-data :type -undefined :initarg :-error-data :initform :undefined)
   (constructor :initform (make-property :value '!error) :allocation :class)
   ;; Message is not a class allocation.
   (message :type property :initform (make-property :value "")
	    :initarg :message)
   (name :type property :allocation :class
	 :initform (make-property :value "Error")))
  (:documentation "Error prototype, provides inherited properties."))

;;; Now we handle all kinds of errors the same way. It can be merged
;;; into Lisp style condition system.
(defclass -eval-error-proto (-error-proto)
  ((-prototype :initform '-error-proto)
   (constructor :initform (make-property :value '!eval-error)
		:allocation :class))
  (:documentation "Evaluation error prototype."))

(defclass -range-error-proto (-error-proto)
  ((-prototype :initform '-error-proto)
   (constructor :initform (make-property :value '!range-error)
		:allocation :class))
  (:documentation "Range error prototype."))

(defclass -reference-error-proto (-error-proto)
  ((-prototype :initform '-error-proto)
   (constructor :initform (make-property :value '!reference-error)
		:allocation :class))
  (:documentation "Reference error prototype."))

(defclass -syntax-error-proto (-error-proto)
  ((-prototype :initform '-error-proto)
   (constructor :initform (make-property :value '!syntax-error)
		:allocation :class))
  (:documentation "Syntax error prototype."))

(defclass -type-error-proto (-error-proto)
  ((-prototype :initform '-error-proto)
   (constructor :initform (make-property :value '!type-error)
		:allocation :class))
  (:documentation "Type error prototype."))

(defclass -uri-error-proto (-error-proto)
  ((-prototype :initform '-error-proto)
   (constructor :initform (make-property :value '!uri-error)
		:allocation :class))
  (:documentation "URI error prototype."))

(defmethod fetch-properties ((this -error-proto))
  (properties (make-instance (class-name this))))

(defmethod fetch-properties ((this -eval-error-proto))
  (properties (make-instance (class-name this))))

(defmethod fetch-properties ((this -range-error-proto))
  (properties (make-instance (class-name this))))

(defmethod fetch-properties ((this -reference-error-proto))
  (properties (make-instance (class-name this))))

(defmethod fetch-properties ((this -syntax-error-proto))
  (properties (make-instance (class-name this))))

(defmethod fetch-properties ((this -type-error-proto))
  (properties (make-instance (class-name this))))

(defmethod fetch-properties ((this -uri-error-proto))
  (properties (make-instance (class-name this))))

(defun -error (message)
  (let* ((msg (slot-value (-to-string message) '-string-data))
	 (msg-pro (make-property :value msg :writable :true
				 :configurable :true)))
    (make-instance '-error-proto :message msg-pro)))

(defmethod print-object ((this -error-proto) stream)
  )

(defmethod print-object ((this -eval-error-proto) stream)
  )

(defmethod print-object ((this -range-error-proto) stream)
  )

(defmethod print-object ((this -reference-error-proto) stream)
  )

(defmethod print-object ((this -syntax-error-proto) stream)
  )

(defmethod print-object ((this -type-error-proto) stream)
  )

(defmethod print-object ((this -uri-error-proto) stream)
  )

(defmethod to-string ((this -error-proto) &optional radix)
  (declare (ignore radix))
  )

(defmethod to-string ((this -eval-error-proto) &optional radix)
  (declare (ignore radix))
  )

(defmethod to-string ((this -range-error-proto) &optional radix)
  (declare (ignore radix))
  )

(defmethod to-string ((this -reference-error-proto) &optional radix)
  (declare (ignore radix))
  )

(defmethod to-string ((this -syntax-error-proto) &optional radix)
  (declare (ignore radix))
  )

(defmethod to-string ((this -type-error-proto) &optional radix)
  (declare (ignore radix))
  )

(defmethod to-string ((this -uri-error-proto) &optional radix)
  (declare (ignore radix))
  )
