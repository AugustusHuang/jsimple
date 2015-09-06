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

;;;; Boolean builtin type definitions.
(in-package :lesp-builtin)

(deftype boolean-raw ()
  `(member :false :true))

(defclass -boolean-prototype (-object-prototype)
  ((-prototype :initform '-object-prototype :allocation :class)
   ;; Extensible is the same.
   (-boolean-data :type boolean-raw :initarg :-boolean-data)
   (constructor :initform (make-property :value '-boolean) :allocation :class)
   (properties
    :initform (fetch-properties (find-class '-object-prototype))
    :allocation :class))
  (:documentation "Boolean prototype, provides inherited properties."))

(defclass -boolean (-function-prototype)
  ((-prototype :initform '-function-prototype :allocation :class)
   ;; Extensible is the same.
   (length :initform (make-property :value 1) :allocation :class)
   (prototype :type (or property -null) :accessor prototype :allocation :class
	      :initarg :prototype
	      :initform (make-property :value '-boolean-prototype))
   (properties
    :initform (fetch-properties (find-class '-function-prototype))
    :allocation :class))
  (:documentation "Boolean constructor, used with new operator."))

(defmethod fetch-properties ((this -boolean-prototype))
  (properties (make-instance (class-name this))))

(defmethod fetch-properties ((this -boolean))
  (properties (make-instance (class-name this))))

(defmethod print-object ((this -boolean-prototype) stream)
  (format stream (if (eql (slot-value this '-boolean-data) :true)
		     "true"
		     "false")))

(defmethod -get-prototype-of ((this -boolean-prototype))
  )

(defmethod -set-prototype-of ((this -boolean-prototype) proto)
  )

(defmethod -is-extensible ((this -boolean-prototype))
  )

(defmethod -prevent-extensions ((this -boolean-prototype))
  )

(defmethod -get-own-property ((this -boolean-prototype) key)
  )

(defmethod -has-property ((this -boolean-prototype) key)
  )

(defmethod -get ((this -boolean-prototype) key receiver)
  )

(defmethod -set ((this -boolean-prototype) key value receiver)
  )

(defmethod -delete ((this -boolean-prototype) key)
  )

(defmethod -define-own-property ((this -boolean-prototype) key descriptor)
  )

(defmethod -enumerate ((this -boolean-prototype))
  )

(defmethod -own-property-keys ((this -boolean-prototype))
  )

(defmethod -call ((this -boolean) &rest args)
  )

(defmethod -construct ((this -boolean) args object)
  )

(defmethod to-string ((this -boolean-prototype) &optional radix)
  (declare (ignore radix))
  (if (eql (slot-value this '-boolean-data) :true)
      "true"
      "false"))

(defmethod value-of ((this -boolean-prototype))
  (slot-value this '-boolean-data))

