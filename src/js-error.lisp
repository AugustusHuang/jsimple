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
(defclass -error-prototype (-object-prototype)
  ((-prototype :initform '-object-prototype)
   (-error-data :type -undefined :initarg :-error-data :initform :undefined)
   (constructor :initform (make-property :value '-error) :allocation :class)
   (properties
    :initform
    (append (fetch-properties (find-class '-object-prototype))
	    '((message . (make-property :value ""))
	      (name . (make-property :value "Error"))))
    :allocation :class))
  (:documentation "Error prototype, provides inherited properties."))

(defclass -error (-function-prototype)
  ((-prototype :initform '-function-prototype)
   (length :initform (make-property :value 1) :accessor length
	   :allocation :class)
   (prototype :type (or property -null) :allocation :class :accessor prototype
	      :initarg :prototype
	      :initform (make-property :value '-error-prototype))
   (properties
    :initform (fetch-properties (find-class '-function-prototype))
    :allocation :class))
  (:documentation "Error constructor."))

;;; Now we handle all kinds of errors the same way. It can be merged
;;; into Lisp style condition system.
(defclass -eval-error-prototype (-error-prototype)
  ((-prototype :initform '-error-prototype)
   (constructor :initform (make-property :value '-eval-error)
		:allocation :class))
  (:documentation "Evaluation error prototype."))

(defclass -eval-error (-error)
  ((-prototype :initform '-error)
   (prototype :initform (make-property :value '-eval-error-prototype)
	      :allocation :class))
  (:documentation "Evaluation error. Currently not used."))

(defclass -range-error-prototype (-error-prototype)
  ((-prototype :initform '-error-prototype)
   (constructor :initform (make-property :value '-range-error)
		:allocation :class))
  (:documentation "Range error prototype."))

(defclass -range-error (-error)
  ((-prototype :initform '-error)
   (prototype :initform (make-property :value '-range-error-prototype)
	      :allocation :class))
  (:documentation "Range error. Indicates a value that is not in the set or
range of allowable values."))

(defclass -reference-error-prototype (-error-prototype)
  ((-prototype :initform '-error-prototype)
   (constructor :initform (make-property :value '-reference-error)
		:allocation :class))
  (:documentation "Reference error prototype."))

(defclass -reference-error (-error)
  ((-prototype :initform '-error)
   (prototype :initform (make-property :value '-reference-error-prototype)
	      :allocation :class))
  (:documentation "Reference error. Indicates that an invalid reference value
has been detected."))

(defclass -syntax-error-prototype (-error-prototype)
  ((-prototype :initform '-error-prototype)
   (constructor :initform (make-property :value '-syntax-error)
		:allocation :class))
  (:documentation "Syntax error prototype."))

(defclass -syntax-error (-error)
  ((-prototype :initform '-error)
   (prototype :initform (make-property :value '-syntax-error-prototype)
	      :allocation :class))
  (:documentation "Syntax error. Indicates that a parsing error has occured."))

(defclass -type-error-prototype (-error-prototype)
  ((-prototype :initform '-error-prototype)
   (constructor :initform (make-property :value '-type-error)
		:allocation :class))
  (:documentation "Type error prototype."))

(defclass -type-error (-error)
  ((-prototype :initform '-error)
   (prototype :initform (make-property :value '-type-error-prototype)
	      :allocation :class))
  (:documentation "Type error. Indicates the actual type of an operand is
different than the expected type."))

(defclass -uri-error-prototype (-error-prototype)
  ((-prototype :initform '-error-prototype)
   (constructor :initform (make-property :value '-uri-error)
		:allocation :class))
  (:documentation "URI error prototype."))

(defclass -uri-error (-error)
  ((-prototype :initform '-error)
   (prototype :initform (make-property :value '-uri-error-prototype)
	      :allocation :class))
  (:documentation "URI error. Indicates that one of the global URI handling
functions was used in a way that is incompatible with its definition."))

(defmethod fetch-properties ((this -error-prototype))
  (properties (make-instance (class-name this))))

(defmethod fetch-properties ((this -error))
  (properties (make-instance (class-name this))))

(defmethod fetch-properties ((this -eval-error-prototype))
  (properties (make-instance (class-name this))))

(defmethod fetch-properties ((this -eval-error))
  (properties (make-instance (class-name this))))

(defmethod fetch-properties ((this -range-error-prototype))
  (properties (make-instance (class-name this))))

(defmethod fetch-properties ((this -range-error))
  (properties (make-instance (class-name this))))

(defmethod fetch-properties ((this -reference-error-prototype))
  (properties (make-instance (class-name this))))

(defmethod fetch-properties ((this -reference-error))
  (properties (make-instance (class-name this))))

(defmethod fetch-properties ((this -syntax-error-prototype))
  (properties (make-instance (class-name this))))

(defmethod fetch-properties ((this -syntax-error))
  (properties (make-instance (class-name this))))

(defmethod fetch-properties ((this -type-error-prototype))
  (properties (make-instance (class-name this))))

(defmethod fetch-properties ((this -type-error))
  (properties (make-instance (class-name this))))

(defmethod fetch-properties ((this -uri-error-prototype))
  (properties (make-instance (class-name this))))

(defmethod fetch-properties ((this -uri-error))
  (properties (make-instance (class-name this))))

(defmethod print-object ((this -error-prototype) stream)
  )

(defmethod print-object ((this -eval-error-prototype) stream)
  )

(defmethod print-object ((this -range-error-prototype) stream)
  )

(defmethod print-object ((this -reference-error-prototype) stream)
  )

(defmethod print-object ((this -syntax-error-prototype) stream)
  )

(defmethod print-object ((this -type-error-prototype) stream)
  )

(defmethod print-object ((this -uri-error-prototype) stream)
  )

(defmethod to-string ((this -error-prototype) &optional radix)
  (declare (ignore radix))
  )

(defmethod to-string ((this -eval-error-prototype) &optional radix)
  (declare (ignore radix))
  )

(defmethod to-string ((this -range-error-prototype) &optional radix)
  (declare (ignore radix))
  )

(defmethod to-string ((this -reference-error-prototype) &optional radix)
  (declare (ignore radix))
  )

(defmethod to-string ((this -syntax-error-prototype) &optional radix)
  (declare (ignore radix))
  )

(defmethod to-string ((this -type-error-prototype) &optional radix)
  (declare (ignore radix))
  )

(defmethod to-string ((this -uri-error-prototype) &optional radix)
  (declare (ignore radix))
  )
