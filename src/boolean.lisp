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
(in-package :jsimple-builtin)

(deftype boolean-raw ()
  `(member :false :true))

(defclass -boolean ()
  ((constructor :reader constructor :type string
		:initarg :constructor :initform "Boolean"
		:allocation class)
   (data :accessor data :type boolean-raw
	 :initarg :data :initform :false))
  (:documentation "Builtin Boolean prototype."))

(defmethod print-object ((this -boolean) stream)
  (format stream (if (eql (data this) :true)
		     "true"
		     "false"))
  this)

;;; new Boolean() constructs a object with [[PrimitiveValue]]: true.
;;; While Boolean() converts some thing to a boolean.
(defun -boolean-constructor (value)
  (-to-boolean value))

(defun new-boolean (value)
  (-object-constructor value))

(defun -to-boolean (value)
  "Abstract operation of some-type to boolean conversion."
  (typecase value
    (-undefined
     (make-instance '-boolean :data :false))
    (-null
     (make-instance '-boolean :data :false))
    (-boolean
     value)
    (-number
     ;; When DATA is 0 or NaN, return FALSE.
     (make-instance '-boolean :data (if (or (eql (data value) :nan)
					      (= (data value) 0))
					:false
					:true)))
    (-string
     (make-instance '-boolean :data :true))
    (-symbol
     (make-instance '-boolean :data :true))
    (-object
     (make-instance '-boolean :data :true))))

(defmethod to-string ((this -boolean))
  (if (eql (data this) :true)
      "true"
      "false"))

(defmethod to-locale-string ((this -boolean))
  (if (eql (data this) :true)
      "true"
      "false"))

(defmethod value-of ((this -boolean))
  (data this))
