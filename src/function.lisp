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

;;; Every function has its own symbol, once created, store it into a global
;;; hashtable in order to prevent duplicates. (Since naming style is quite
;;; different in Lisp and ES, functions like Another and another will be the
;;; same... But they are different, if use thisIsAFunctionStyle, it will
;;; be parsed into this-is-a-function-style in Lisp, and if use ThisType,
;;; it will be parsed into !this-type, and then the problem will be trivial...
;;; See builtin-util.lisp

(defclass -function ()
  ((constructor :reader constructor :type string
		:initarg :constructor :initform "Function")
   (data :accessor data :type string
	 :initarg :data :initform "Anonymous"))
  (:documentation "Builtin function prototype."))

(defun -function-constructor (name &rest args definition)
  )

(defmethod to-string ((this -function))
  )

(defmethod to-locale-string ((this -function))
  )

(defmethod value-of ((this -function))
  )
