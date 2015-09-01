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
;;; same... But they are different, if all use thisIsAFunctionStyle, it will
;;; be parsed into this-is-a-function-style in Lisp, and the problem will
;;; be trivial...

;;; NOTE: An assoc list will be better, but now we use this.
(defparameter *functions-table*
  (let ((funcs (make-hash-table :test 'equal)))
    (dolist (name '("Function" "Object" "Number" "Boolean" "String"
		    "Symbol" "Error" "Int8Array" "Int16Array" "Int32Array"
		    "Uint8Array" "Uint8ClampedArray" "Uint16Array"
		    "Uint32Array" "Float32Array" "Float64Array" "URI"
		    "EvalError" "RangeError" "ReferenceError" "SyntaxError"
		    "TypeError" "URIError" "Array" "ArrayBuffer" "Date"
		    "DataView" "JSON" "Map" "Math" "RegExp" "Set" "WeakMap"
		    "WeakSet"))
      (setf (gethash name funcs) t))
    funcs))

(defun not-duplicate (name)
  (gethash name *functions-table*))
		    
(deftype js-function-raw ()
  `(and string (satisfies not-duplicate)))

(defclass js-function ()
  ((constructor :reader constructor :type js-function-raw
		:initarg :constructor :initform 'function)
   (data :accessor data :type js-function-raw
	 :initarg :data :initform 'anonymous))
  (:documentation "Builtin function prototype."))

(defun js-function-constructor (name &rest args definition)
  )

(defmethod js-to-string ((this js-function))
  )

(defmethod js-to-locale-string ((this js-function))
  )

(defmethod js-value-of ((this js-function))
  )
