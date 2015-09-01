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

(deftype js-boolean-raw ()
  `(member :false :true))

(defclass js-boolean ()
  ((constructor :reader constructor :type js-function-raw
		:initarg :constructor :initform 
   (data :accessor data :type js-boolean-raw
	 :initarg :data :initform :false))
  (:documentation "Builtin Boolean prototype."))

(defmethod js-intern-data ((this js-boolean))
  (slot-value this 'data))

(defun js-boolean-constructor (value)
  (typecase value
    (js-undefined
     (make-instance 'js-boolean :data :false))
    (js-null
     (make-instance 'js-boolean :data :false))
    (js-boolean
     (make-instance 'js-boolean :data (js-intern-data value)))
    (js-number
     ;; When DATA is 0 or NaN, return FALSE.
     (make-instance 'js-boolean :data (if (or (eql (js-intern-data value) :nan)
					      (= (js-intern-data value) 0))
					  :false
					  :true)))
    (js-string
     (make-instance 'js-boolean :data :true))
    (js-symbol
     (make-instance 'js-boolean :data :true))
    (js-object
     (make-instance 'js-boolean :data :true))))

(defmethod js-to-string ((this js-boolean))
  (if (eql (js-intern-data this) :true)
      "true"
      "false"))

(defmethod js-to-locale-string ((this js-boolean))
  (if (eql (js-intern-data this) :true)
      "true"
      "false"))

(defmethod js-value-of ((this js-boolean))
  (js-intern-data this))
