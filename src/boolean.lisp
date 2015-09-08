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

(defclass -boolean-proto (-object-proto)
  ((-prototype :initform (find-class '-object-proto))
   ;; Extensible is the same.
   (-boolean-data :type boolean-raw :initarg :-boolean-data
		  :initform :false)
   (constructor :initform (make-property :value -boolean) :allocation :class))
  (:documentation "Boolean prototype, provides inherited properties."))

(defun -to-boolean (arg)
  (typecase arg
    (undefined-raw
     (-boolean :false))
    (null-raw
     (-boolean :false))
    (-boolean-proto
     arg)
    (-number-proto
     (if (or (= 0 (slot-value arg '-number-data))
	     (= :nan (slot-value arg '-number-data)))
	 (-boolean :false)
	 (-boolean :true)))
    (-string-proto
     (-boolean :true))
    (-symbol-proto
     (-boolean :true))
    (-object-proto
     (-boolean :true))))

(defmethod fetch-properties ((this -boolean-proto))
  (properties (make-instance (class-name this))))

(defmethod print-object ((this -boolean-proto) stream)
  (format stream (if (eql (slot-value this '-boolean-data) :true)
		     "true"
		     "false")))

(defmethod -get-prototype-of ((this -boolean-proto))
  )

(defmethod -set-prototype-of ((this -boolean-proto) proto)
  )

(defmethod -is-extensible ((this -boolean-proto))
  )

(defmethod -prevent-extensions ((this -boolean-proto))
  )

(defmethod -get-own-property ((this -boolean-proto) key)
  )

(defmethod -has-property ((this -boolean-proto) key)
  )

(defmethod -get ((this -boolean-proto) key receiver)
  )

(defmethod -set ((this -boolean-proto) key value receiver)
  )

(defmethod -delete ((this -boolean-proto) key)
  )

(defmethod -define-own-property ((this -boolean-proto) key descriptor)
  )

(defmethod -enumerate ((this -boolean-proto))
  )

(defmethod -own-property-keys ((this -boolean-proto))
  )

(defmethod to-string ((this -boolean-proto) &optional radix)
  (declare (ignore radix))
  (if (eql (slot-value this '-boolean-data) :true)
      "true"
      "false"))

(defmethod value-of ((this -boolean-proto))
  (slot-value this '-boolean-data))

