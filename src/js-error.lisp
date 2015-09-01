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

(in-package :jsimple-builtin)

;;;; Error related definitions and methods.

(defclass js-error ()
  ((message :initarg :message :type string :initform "" :accessor message)
   (name :initarg :name :type string :initform "Error" :accessor name))
  (:documentation "Error prototype, will be the superclass of all errors."))

(defmethod js-message ((this js-error))
  (slot-value this 'message))

(defmethod js-name ((this js-error))
  (slot-value this 'name))

(defmethod js-to-string ((this js-error))
  (concatenate 'string (js-name this) ": " (js-message this)))

;;; Now we handle all kinds of errors the same way. But it can be merged
;;; into Lisp style condition system. Someday it will do.
(defclass js-eval-error (js-error)
  ()
  (:documentation "Evaluation error. Currently not used."))

(defclass js-range-error (js-error)
  ()
  (:documentation "Range error. Indicates a value that is not in the set or
range of allowable values."))

(defclass js-reference-error (js-error)
  ()
  (:documentation "Reference error. Indicates that an invalid reference value
has been detected."))

(defclass js-syntax-error (js-error)
  ()
  (:documentation "Syntax error. Indicates that a parsing error has occured."))

(defclass js-type-error (js-error)
  ()
  (:documentation "Type error. Indicates the actual type of an operand is
different than the expected type."))

(defclass js-uri-error (js-error)
  ()
  (:documentation "URI error. Indicates that one of the global URI handling
functions was used in a way that is incompatible with its definition."))
