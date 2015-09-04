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

;;; In ECMA-262, internal slots are not object properties and they are not
;;; inherited, and they are allocated as part of the process of creating an
;;; object and may not be dynamically added to an object.

;;; An ordinary object is made up by a prototype part and an internal part,
;;; prototype part is inherited as prototype, but internal part is only
;;; accessible by internal methods or print methods, users can't alter them.

;;; %ObjectPrototype% Object Prototype Object: [[Prototype]] = null,
;;; [[Extensible]] = true, constructor = %Object%, hasOwnProperty = t,
;;; isPrototypeOf = t, propertyIsEnumerable = t, toLocaleString = t,
;;; toString = t, valueOf = t.
(defclass -object-prototype ()
  ((prototype :type (or symbol -null)
	      :initarg :prototype
	      :initform :null)
   (extensible :type (or boolean-raw -undefined)
	       :initarg :extensible
	       :iniform :true)
   (own-properties :accessor own :type (or null list)
		   :initarg :own
		   ;; If we want to refer to a class, use symbol name, and
		   ;; use FIND-CLASS to get the corresponding class.
		   :initform '((constructor . -object) (has-own-property . t)
			       (is-prototype-of . t)
			       (property-is-enumerable . t)
			       (to-locale-string . t) (to-string . t)
			       (value-of . t)))
   (inherit-properties :accessor inherit :type (or null list)
		       :initarg :inherit
		       :initform nil))
  (:documentation "Object prototype, provides inherited properties."))

;;; %Object% Object Constructor: [[Prototype]] = %FunctionPrototype%,
;;; [[Extensible]] = undefined, length = 1, assign = t, create = t,
;;; defineProperties = t, defineProperty = t, freeze = t,
;;; getOwnPropertyDescriptor = t, getOwnPropertyNames = t,
;;; getOwnPropertySymbols = t, getPrototypeOf = t, is = t, isExtensible = t,
;;; isFrozen = t, isSealed = t, keys = t, preventExtensions = t, seal = t,
;;; prototype = %ObjectPrototype%, seal = t, setPrototypeOf = t.
(defclass -object ()
  ((prototype :type (or symbol -null)
	      :initarg :prototype :initform '-function)
   (extensible :type (or boolean-raw -undefined)
	       :initarg :extensible
	       :initform :true)
   (properties :accessor properties :type (or null list)
	       :initarg :properties
	       :initform '((length . 1) (prototype . -object-prototype)
			   (assign . t) (create . t) (define-properties . t)
			   (define-property . t) (freeze . t)
			   (get-own-property-descriptor . t)
			   (get-own-property-names . t)
			   (get-own-property-symbols . t)
			   (get-prototype-of . t)
			   (is . t) (is-extensible . t) (is-frozen . t)
			   (is-sealed . t) (keys . t) (prevent-extensions . t)
			   (seal . t) (set-prototype-of . t))))
  (:documentation "Object constructor, used with new operator."))

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

(defun -to-object (value)
  "Abstract operation of some-type to object conversion."
  (typecase value
    (-undefined
     (error '-type-error))
    (-null
     (error '-type-error))
    (-boolean
     (make-instance '-object-boolean :boolean-data (data value)))
    (-number
     (make-instance '-object-number :number-data (data value)))
    (-string
     (make-instance '-object-string
		    :properties (string-to-object-properties (data value))
		    :string-data (data value)))
    (-symbol
     (make-instance '-object-symbol :symbol-data (data value)))
    ;; Won't be here.
    (t
     (error '-type-error))))

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
