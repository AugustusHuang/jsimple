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

;;; In ECMA-262, internal slots are not object properties and they are not
;;; inherited, and they are allocated as part of the process of creating an
;;; object and may not be dynamically added to an object.

;;; An ordinary object is made up by a prototype part and an internal part,
;;; prototype part is inherited as prototype, but internal part is only
;;; accessible by internal methods or print methods, users can't alter them
;;; barely by using accessor functions. e.g. [[Prototype]] can only be modified
;;; by [[SetPrototypeOf]] internal methods.

;;; In this very internal stage, we don't tell the different between internal
;;; slots and properties that shared everywhere like prototype, constructor
;;; and length. The different between them is that internal slots contain
;;; bare-bone data, but properties contain PROPERTY typed data. Method
;;; properties are directly handled by CLOS, And every method property will
;;; have a lot of other properties, so they will be implemented as built-in
;;; function instances of built-in function class.

;;; %ObjectPrototype% Object Prototype Object: [[Prototype]] = null,
;;; [[Extensible]] = true, constructor = %Object%, hasOwnProperty = t,
;;; isPrototypeOf = t, propertyIsEnumerable = t, toLocaleString = t,
;;; toString = t, valueOf = t.
(defclass -object-proto (proto)
  ((-prototype :initform :null)
   (-extensible :initform :true)
   (has-own-property :type property :initarg :has-own-property
		     :allocation :class
		     :initform
		     (make-property :value (-builtin-function #'has-own-property)))
   (is-prototype-of :type property :initarg :is-prototype-of :allocation :class
		    :initform
		    (make-property :value (-builtin-function #'is-prototype-of)))
   (property-is-enumerable :type property :initarg :property-is-enumerable
			   :allocation :class
			   :initform
			   (make-property :value (-builtin-function #'property-is-enumerable)))
   (to-locale-string :type property :initarg :to-locale-string
		     :allocation :class
		     :initform
		     (make-property :value (-builtin-function #'to-locale-string)))
   (to-string :type property :initarg :to-string :allocation :class
	      :initform
	      (make-property :value (-builtin-function #'to-string)))
   (value-of :type property :initarg :value-of :allocation :class
	     :initform
	     (make-property :value (-builtin-function #'value-of)))
   (properties :type list :initarg :properties
	       :initform
	       '((constructor . (make-property :value -object)))))
  (:documentation "Object prototype, provides inherited properties."))

;;; %Object% Object Constructor: [[Prototype]] = %FunctionPrototype%,
;;; [[Extensible]] = undefined, length = 1, assign = t, create = t,
;;; defineProperties = t, defineProperty = t, freeze = t,
;;; getOwnPropertyDescriptor = t, getOwnPropertyNames = t,
;;; getOwnPropertySymbols = t, getPrototypeOf = t, is = t, isExtensible = t,
;;; isFrozen = t, isSealed = t, keys = t, preventExtensions = t, seal = t,
;;; prototype = %ObjectPrototype%, seal = t, setPrototypeOf = t.

;;; Helpers to make access to PROPERTIES when
;;; we are creating new class objects.
(defmethod fetch-properties ((this -object-prototype))
  (properties (make-instance (class-name this))))

;;; We need PRINT-OBJECT methods for all mixins and original object type.
;;; Object style: <Object: <a: 1> <b: 2>>
;;; Boolean object style: <Boolean: <primitive-value: true>>
;;; Number object style: <Number: <primitive-value: 3>>
;;; String object style:
;;; <String: <0: "s"> <1: "t"> <2: "r"> <length: 3> <primitive-value: "str">>
;;; Symbol object style: <Symbol: <primitive-value: 'sym>>.
(defmethod print-object ((this -object-prototype) stream)
  ;; ALIST looks like ((a . b) (c . d) (e . f)).
  ;; Make it into form ((a b) (c d) (e f)).
  (labels ((pair-out (lst)
	     (let ((result ()))
	       (loop for pair in lst do
		    (push (list (car pair) (cdr pair)) result))
	       result)))
    (format stream "<Object:~:{ <~S: ~S>~}>"
	    (pair-out (properties this)))))

;;; To make string slots properties.
(defun string-to-object-properties (string)
  "Converts a string to an assoc-list with keys indices and values chars, followed with length of string."
  (let ((alist ())
	(len (length string)))
    (loop for i from 0 to (1- len) do
	 ;; There's no raw chars in ES, make chars strings instead.
	 (setf alist
	       (acons (write-to-string i) (string (char string i)) alist)))
    (setf alist (acons "length" len alist))
    (reverse alist)))

;;; Internal methods.
(defmethod -get-prototype-of ((this -object-prototype))
  )

(defmethod -set-prototype-of ((this -object-prototype) proto)
  )

(defmethod -is-extensible ((this -object-prototype))
  )

(defmethod -prevent-extensions ((this -object-prototype))
  )

(defmethod -get-own-property ((this -object-prototype) key)
  )

(defmethod -has-property ((this -object-prototype) key)
  )

(defmethod -get ((this -object-prototype) key receiver)
  )

(defmethod -set ((this -object-prototype) key value receiver)
  )

(defmethod -delete ((this -object-prototype) key)
  )

(defmethod -define-own-property ((this -object-prototype) key descriptor)
  )

(defmethod -enumerate ((this -object-prototype))
  )

(defmethod -own-property-keys ((this -object-prototype))
  )

;;; Object property methods and Object prototype property methods.
;;; Since Object is called without THIS, define them as functions...
(defun assign (target &rest sources)
  )

(defun create (object &optional property-list)
  )

(defun define-properties (object property-list)
  )

(defun define-property (object property attributes)
  )

(defun freeze (object)
  )

(defun get-own-property-descriptor (object property)
  )

(defun get-own-property-names (object)
  )

(defun get-own-property-symbols (object)
  )

(defun get-prototype-of (object)
  )

(defun is (value1 value2)
  )

(defun is-extensible (object)
  )

(defun is-frozen (object)
  )

(defun is-sealed (object)
  )

(defun keys (object)
  )

(defun prevent-extensions (object)
  )

(defun seal (object)
  )

(defun set-prototype-of (object prototype)
  )

;;; Prototype property methods are handled with 'this', so define them as
;;; methods.
(defmethod has-own-property ((this -object-prototype) value)
  )

(defmethod is-prototype-of ((this -object-prototype) value)
  )

(defmethod property-is-enumerable ((this -object-prototype) value)
  )

(defmethod to-locale-string ((this -object-prototype))
  )

(defmethod to-string ((this -object-prototype) &optional radix)
  (declare (ignore radix))
  )

(defmethod value-of ((this -object-prototype))
  )
