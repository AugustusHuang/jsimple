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

;;;; Builtin string type definitions.
(in-package :jsimple-builtin)

(deftype string-raw ()
  'string)

(defclass -string-prototype (-object-prototype)
  ()
  (:documentation "String prototype, provides inherited properties."))

(defclass -string (-function-prototype)
  ()
  (:documentation "String constructor, used with new operator."))

(defun -new-string (value)
  (-new-object-string value))

(defun -to-string (value)
  (typecase value
    (-undefined
     (make-instance '-string :data "undefined"))
    (-null
     (make-instance '-string :data "null"))
    (-boolean
     (make-instance '-string :data (if (eql (data value) :false)
				       "false"
				       "true")))
    (-number
     (make-instance '-string :data (if (eql (data value) :nan)
				       "NaN"
				       (write-to-string (data value)))))
    (-string
     value)
    (-symbol
     (error '-type-error))
    (-object
     (make-instance '-string :data (-to-string (data value))))))

(defmethod fetch-properties ((this -string-prototype))
  (properties (make-instance (class-name this))))

(defmethod fetch-properties ((this -string))
  (properties (make-instance (class-name this))))

(defmethod to-string ((this -string-prototype))
  )

(defmethod to-locale-string ((this -string-prototype))
  )

(defmethod value-of ((this -string-prototype))
  )
