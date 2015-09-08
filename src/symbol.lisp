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

;;;; Builtin symbol type definitions.
(in-package :lesp-builtin)

;;; Well known symbols are built-in symbol values are typically used
;;; as the keys of properties. -- ECMA V6.
(defclass -symbol-proto (-object-proto)
  ((-prototype :initform (find-class '-object-proto))
   (-symbol-data :type symbol-raw :initarg :-symbol-data)
   (constructor :initform (make-property :value -symbol) :allocation :class))
  (:documentation "Symbol prototype, provides inherited properties."))

(defmethod print-object ((this -symbol-proto) stream)
  )

(defmethod -get-prototype-of ((this -symbol-proto))
  )

(defmethod -set-prototype-of ((this -symbol-proto) proto)
  )

(defmethod -is-extensible ((this -symbol-proto))
  )

(defmethod -prevent-extensions ((this -symbol-proto))
  )

(defmethod -get-own-property ((this -symbol-proto) key)
  )

(defmethod -has-property ((this -symbol-proto) key)
  )

(defmethod -get ((this -symbol-proto) key receiver)
  )

(defmethod -set ((this -symbol-proto) key value receiver)
  )

(defmethod -delete ((this -symbol-proto) key)
  )

(defmethod -define-own-property ((this -symbol-proto) key descriptor)
  )

(defmethod -enumerate ((this -symbol-proto))
  )

(defmethod -own-property-keys ((this -symbol-proto))
  )

(defmethod to-string ((this -symbol-proto) &optional radix)
  (declare (ignore radix))
  )

(defmethod value-of ((this -symbol-proto))
  )

(defmethod to-primitive ((this -symbol-proto) hint)
  )

