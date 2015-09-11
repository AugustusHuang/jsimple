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
   (-primitive-value :initform :undefined)
   (constructor :initform (make-property :value -object) :allocation :class)
   (has-own-property :type property :allocation :class
		     :initform
		     (make-property :value (-builtin-function #'has-own-property)))
   (is-prototype-of :type property :allocation :class
		    :initform
		    (make-property :value (-builtin-function #'is-prototype-of)))
   (property-is-enumerable :type property :allocation :class
			   :initform
			   (make-property :value (-builtin-function #'property-is-enumerable)))
   (to-locale-string :type property :allocation :class
		     :initform
		     (make-property :value (-builtin-function #'to-locale-string)))
   (to-string :type property :allocation :class
	      :initform (make-property :value (-builtin-function #'to-string)))
   (value-of :type property :allocation :class
	     :initform (make-property :value (-builtin-function #'value-of)))
   (properties :initform nil))
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
(defmethod fetch-properties ((this -object-proto))
  (properties (make-instance (class-name this))))

;;; We need PRINT-OBJECT methods for all mixins and original object type.
;;; Object style: <Object: <a: 1> <b: 2>>
;;; Boolean object style: <Boolean: <primitive-value: true>>
;;; Number object style: <Number: <primitive-value: 3>>
;;; String object style:
;;; <String: <0: "s"> <1: "t"> <2: "r"> <length: 3> <primitive-value: "str">>
;;; Symbol object style: <Symbol: <primitive-value: 'sym>>.
(defmethod print-object ((this -object-proto) stream)
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

(defun -to-object (arg)
  (typecase arg
    (undefined-type
     (error "Invalid conversion from undefined to object"))
    (null-type
     (error "Invalid conversion from null to object"))
    (boolean-type
     (-object :-primitive-value (slot-value arg 'boolean-data)))
    (number-type
     (-object :-primitive-value (slot-value arg 'number-data)))
    (string-type
     (-object :-primitive-value (slot-value arg 'string-data)))
    (symbol-type
     (-object :-primitive-value (slot-value arg 'symbol-data)))
    (object-type
     arg)))

;;; Internal methods.
(defmethod -get-prototype-of ((this -object-proto))
  (slot-value this '-prototype))

;;; PROTO is a class object or :NULL.
(defmethod -set-prototype-of ((this -object-proto) proto)
  (assert (or (eql (-type proto) 'object-type)
	      (eql proto :null))
	  (this proto)
	  "PROTO is not of type Object or Null.")
  (let ((extensible (slot-value this '-extensible))
	(current (slot-value this '-prototype)))
    (when (eql proto current)
      *boolean-true*)
    (when (eql extensible :false)
      *boolean-false*)
    (let ((done nil)
	  (p proto))
      (loop
	 (when done (return))
	 (if (eql p :null)
	     (setf done t)
	     (if (eql p (class-of this))
		 (return-from -set-prototype-of *boolean-false*)
		 (setf p (-get-prototype-of p)))))
      ;; Of course we should change the class in Lisp land.
      (setf (slot-value this '-prototype) p)
      *boolean-true*)))

(defmethod -is-extensible ((this -object-proto))
  (slot-value this '-extensible))

(defmethod -prevent-extensions ((this -object-proto))
  (progn
    (setf (slot-value this '-extensible) :false)
    *boolean-true*))

(defmethod -get-own-property ((this -object-proto) key)
  (assert (or (eql (-type key) 'string-type)
	      (eql (-type key) 'symbol-type))
	  (this key)
	  "KEY is not a valid property key.")
  (cdr (find-property this key)))

(defmethod -has-property ((this -object-proto) key)
  (let ((desc (-get-own-property this key)))
    (if (not (eql desc :undefined))
	;; Since every property is naturally inherited, we have no need
	;; to check the superclasses.
	*boolean-true*
	*boolean-false*)))

(defmethod -get ((this -object-proto) key receiver)
  (let ((desc (-get-own-property this key)))
    (if (eql desc :undefined)
	;; Also we don't need to check the superclasses.
	:undefined
	;; This property is an accessor property or data property.
	(if (null (property-value desc))
	    (let ((getter (property-get desc)))
	      (if (eql getter :undefined)
		  :undefined
		  (!call getter receiver)))
	    (property-value desc)))))

(defmethod -set ((this -object-proto) key value receiver)
  (let* ((current (find-property this key))
	 (desc (cdr current)))
    (if (eql desc :undefined)
	(setf desc (make-property :writable :true :enumerable :true
				  :configurable :true))
	(if (null (property-value desc))
	    (let ((setter (property-set desc)))
	      (if (eql setter :undefined)
		  *boolean-false*
		  (progn
		    (!call setter receiver value)
		    *boolean-true*)))
	    (progn
	      (when (eql (property-writable desc) :false)
		*boolean-false*)
	      (when (not (eql (-type receiver) 'object-type))
		*boolean-false*)
	      (let ((exist (-get-own-property receiver key)))
		(if (not (eql exist :undefined))
		    (progn
		      (when (not (null (property-set exist)))
			*boolean-false*)
		      (when (eql (property-writable exist) :false)
			*boolean-false*)
		      (return-from -set
			(-define-own-property receiver key (make-property :value value))))
		    (return-from -set
		      (-define-own-property
		       receiver key (make-property :value value
						   :writable :true
						   :enumerable :true
						   :configurable :true))))))))))

(defmethod -delete ((this -object-proto) key)
  (let ((desc (-get-own-property this key)))
    (when (eql desc :undefined)
      *boolean-true*)
    (when (eql (property-configurable desc) :true)
      (remove-property this key)
      *boolean-true*)
    *boolean-false*))

(defmethod -define-own-property ((this -object-proto) key descriptor)
  (declare (type property descriptor))
  ;; Extension is not allowed, return.
  (when (not (slot-value this '-extensible))
    *boolean-false*)
  (let* ((current (find-property this key))
	 (pro (cdr current)))
    (if (eql pro :undefined)
	;; We don't have this property name yet, create a new one.
	(progn
	  (push '(key . descriptor) (slot-value this 'properties))
	  *boolean-true*)
	;; We have this property, then test its fields.
	(if (eql (property-configurable pro) :false)
	    *boolean-false*
	    ;; We can change this.
	    (progn
	      (setf (cdr current) descriptor)
	      *boolean-true*)))))

;;; Return an iterator object whose NEXT method iterates over all the
;;; string-valued keys of enumerable properties of THIS.
(defmethod -enumerate ((this -object-proto))
  )

(defmethod -own-property-keys ((this -object-proto))
  (let ((global-symbol-keys nil)
	(slots (class-slots (class-of this)))
	(assoc-list (slot-value this 'properties)))
    ;; Firstly the internal properties, without those internal slots.
    (loop for i in slots
       if (not (char= #\- (char (symbol-name (slot-definition-name i)) 0)))
       collect (slot-definition-name i) into symbol-keys
       finally (setf global-symbol-keys symbol-keys))
    (setf global-symbol-keys (reverse global-symbol-keys))
    ;; And then the property associative list.
    (loop for (j . property) in assoc-list
       if (not (eql (-to-number j) *number-nan*))
       collect j into integer-keys
       else
       if (eql (-type j) 'string-type)
       collect j into string-keys
       else
       collect j into symbol-keys
       finally
	 (progn
	   (setf global-symbol-keys (append global-symbol-keys symbol-keys))
	   (return-from -own-property-keys
		 (append integer-keys string-keys global-symbol-keys))))))

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
(defmethod has-own-property ((this -object-proto) value)
  )

(defmethod is-prototype-of ((this -object-proto) value)
  )

(defmethod property-is-enumerable ((this -object-proto) value)
  )

(defmethod to-locale-string ((this -object-proto))
  )

(defmethod to-string ((this -object-proto) &optional radix)
  (declare (ignore radix))
  )

(defmethod value-of ((this -object-proto))
  )
