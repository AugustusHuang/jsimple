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
  ((proto :initform :null)
   (-prototype :initform (find-class '-object-proto))
   (-extensible :initform :true)
   (-primitive-value :initform :undefined)
   (constructor :initform (make-property :value '!object) :allocation :class)
   (has-own-property :type property :allocation :class
		     :initform (make-property :value !has-own-property))
   (is-prototype-of :type property :allocation :class
		    :initform (make-property :value !is-prototype-of))
   (property-is-enumerable :type property :allocation :class
			   :initform
			   (make-property :value !property-is-enumerable))
   (to-locale-string :type property :allocation :class
		     :initform (make-property :value !to-locale-string))
   (to-string :type property :allocation :class
	      :initform (make-property :value !to-string))
   (value-of :type property :allocation :class
	     :initform (make-property :value !value-of)))
  (:documentation "Object prototype, provides inherited properties."))

;;; %Object% Object Constructor: [[Prototype]] = %FunctionPrototype%,
;;; [[Extensible]] = undefined, length = 1, assign = t, create = t,
;;; defineProperties = t, defineProperty = t, freeze = t,
;;; getOwnPropertyDescriptor = t, getOwnPropertyNames = t,
;;; getOwnPropertySymbols = t, getPrototypeOf = t, is = t, isExtensible = t,
;;; isFrozen = t, isSealed = t, keys = t, preventExtensions = t, seal = t,
;;; prototype = %ObjectPrototype%, seal = t, setPrototypeOf = t.

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
     (!object :-primitive-value (slot-value arg 'boolean-data)))
    (number-type
     (!object :-primitive-value (slot-value arg 'number-data)))
    (string-type
     (!object :-primitive-value (slot-value arg 'string-data)))
    (symbol-type
     (!object :-primitive-value (slot-value arg 'symbol-data)))
    (object-type
     arg)))

;;; Internal methods.
;;; Here internal methods are handled in Lisp land, while abstract operations
;;; are handled in es land.
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
      (return-from -set-prototype-of :true))
    (when (eql extensible :false)
      (return-from -set-prototype-of :false))
    (let ((done nil)
	  (p proto))
      (loop
	 (when done (return))
	 (if (eql p :null)
	     (setf done t)
	     (if (eql p (class-of this))
		 (return-from -set-prototype-of :false)
		 (setf p (-get-prototype-of p)))))
      ;; FIXME: Of course we should change the class in Lisp land.
      (setf (slot-value this '-prototype) p)
      :true)))

(defmethod -is-extensible ((this -object-proto))
  (slot-value this '-extensible))

(defmethod -prevent-extensions ((this -object-proto))
  (progn
    (setf (slot-value this '-extensible) :false)
    :true))

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
	:true
	:false)))

(defmethod -get ((this -object-proto) key &optional receiver)
  (let ((desc (-get-own-property this key)))
    (if (eql desc :undefined)
	;; Also we don't need to check the superclasses.
	:undefined
	;; This property is an accessor property or data property.
	(if (null (property-value desc))
	    (let ((getter (property-get desc)))
	      (if (eql getter :undefined)
		  (return-from -get :undefined)
		  (!call getter receiver)))
	    (property-value desc)))))

(defmethod -set ((this -object-proto) key value &optional receiver)
  (let* ((current (find-property this key))
	 (desc (cdr current)))
    (if (eql desc :undefined)
	(setf desc (make-property :writable :true :enumerable :true
				  :configurable :true))
	(if (null (property-value desc))
	    (let ((setter (property-set desc)))
	      (if (eql setter :undefined)
		  :false
		  (progn
		    (!call setter receiver value)
		    :true)))
	    (progn
	      (when (eql (property-writable desc) :false)
		(return-from -set :false))
	      (when (not (eql (-type receiver) 'object-type))
		(return-from -set :false))
	      (let ((exist (-get-own-property receiver key)))
		(if (not (eql exist :undefined))
		    (progn
		      (when (not (null (property-set exist)))
			(return-from -set :false))
		      (when (eql (property-writable exist) :false)
			(return-from -set :false))
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
      (return-from -delete :true))
    (when (eql (property-configurable desc) :true)
      (remove-property this key)
      (return-from -delete :true))
    :false))

(defmethod -define-own-property ((this -object-proto) key descriptor)
  (declare (type property descriptor))
  ;; Extension is not allowed, return.
  (when (not (slot-value this '-extensible))
    (return-from -define-own-property :false))
  (let* ((current (find-property this key))
	 (pro (cdr current)))
    (if (eql pro :undefined)
	;; We don't have this property name yet, create a new one.
	(progn
	  (acons key descriptor (slot-value this 'properties))
	  :true)
	;; We have this property, then test its fields.
	(if (eql (property-configurable pro) :false)
	    :false
	    ;; We can change this.
	    (progn
	      (setf (cdr current) descriptor)
	      :true)))))

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
(defun -object.assign (target &rest sources)
  "The function is used to copy the values of all of the enumerable own properties from one or more source objects to a TARGET object."
  (when (eql sources nil)
    (return-from -object.assign target))
  (let ((to (-to-object target)))
    (loop for next in sources
       for keys = ()
       if (not (or (eql next :undefined) (eql next :null)))
       do (let ((from (-to-object next)))
	    (setf keys (-own-property-keys from))
	    (loop for key in keys
	       for desc = (-get-own-property from key)
	       if (and (not (eql desc :undefined))
		       (eql (property-enumerable desc) :true))
	       do (let ((prop (-get from key)))
		    (-set to key prop :true)))))
    to))

(defun object-define-properties (object properties)
  (let* ((props (-to-object properties))
	 (keys (-own-property-keys props))
	 (descriptors ()))
    (loop for next in keys
       for prop-desc = (-get-own-property props next)
       if (and (not (eql prop-desc :undefined))
	       (eql (property-enumerable prop-desc) :true))
       do (let ((desc (-get props next)))
	    (acons next desc descriptors)))
    (loop for every in descriptors
       for key = (car every)
       for desc = (cdr every)
       do (-define-own-property object key desc))
    object))

(defun -object.create (object &optional properties)
  "The function creates a new object with a specified prototype."
  (when (not (or (eql (-type object) 'object-type)
		 (eql (-type object) 'null-type)))
    (error "Type error."))
  (if (null properties)
      object
      (object-define-properties object properties)))

(defun -object.define-properties (object properties)
  "The function is used to add own properties and/or update the attributes of existing own properties of an object."
  (object-define-properties object properties))

(defun -object.define-property (object property attributes)
  "The function is used to add an own property and/or update the attributes of an existing own property of an object."
  (when (not (eql (-type object) 'object-type))
    (error "Type error."))
  (let ((key (-to-property-key property))
	(desc (-to-property-descriptor attributes)))
    (-define-own-property object key desc)
    object))

(defun -object.freeze (object)
  (if (not (eql (-type object) 'object-type))
      object
      (progn
	(-set-integrity-level object "frozen")
	object)))

(defun -object.get-own-property-descriptor (object property)
  (let* ((obj (-to-object object))
	 (key (-to-property-key property))
	 (desc (-get-own-property obj key)))
    (-from-property-descriptor desc)))

(defun -get-own-property-keys (o type)
  ())

(defun -object.get-own-property-names (object)
  (-get-own-property-keys object 'string-type))

(defun -object.get-own-property-symbols (object)
  (-get-own-property-keys object 'symbol-type))

(defun -object.get-prototype-of (object)
  (-get-prototype-of object))

(defun same-value (x y)
  (when (not (eql (-type x) (-type y)))
    (return-from same-value *boolean-false*))
  (typecase x
    (undefined-type
     *boolean-true*)
    (null-type
     *boolean-true*)
    (number-type
     (if (eql x y)
	 *boolean-true*
	 (if (= (slot-value x '-number-data) (slot-value y '-number-data))
	     *boolean-true*
	     *boolean-false*)))
    (string-type
     (if (string= (slot-value x '-string-data) (slot-value y '-string-data))
	 *boolean-true*
	 *boolean-false*))
    (boolean-type
     (if (eql x y)
	 *boolean-true*
	 *boolean-false*))
    (symbol-type
     (if (eql (slot-value x '-symbol-data) (slot-value y '-symbol-data))
	 *boolean-true*
	 *boolean-false*))
    (object-type
     (if (eql x y)
	 *boolean-true*
	 *boolean-false*))))

(defun -object.is (value1 value2)
  (same-value value1 value2))

(defun -object.is-extensible (object)
  (if (eql (-is-extensible object) :false)
      *boolean-false*
      *boolean-true*))

(defun -object.is-frozen (object)
  (if (not (eql (-type object) 'object-type))
      *boolean-true*
      (if (eql (-test-integrity-level object "frozen") :false)
	  *boolean-false*
	  *boolean-true*)))

(defun -object.is-sealed (object)
  (if (not (eql (-type object) 'object-type))
      *boolean-true*
      (if (eql (-test-integrity-level object "sealed") :false)
	  *boolean-false*
	  *boolean-true*)))

(defun -object.keys (object)
  (let ((obj (-to-object object))
	(name-list nil))
    (dolist (key-value (properties obj))
      (let ((prop (cdr key-value))
	    (key (car key-value)))
	(when (eql (property-enumerable prop) :true)
	  ;; Here the key are raw Lisp symbols, how to handle them?
	  (push key name-list))))
    (!array name-list)))

(defun -object.prevent-extensions (object)
  (if (not (eql (-type object) 'object-type))
      object
      (progn
	(-prevent-extensions object)
	object)))

(defun -object.seal (object)
  (if (not (eql (-type object) 'object-type))
      object
      (progn
	(-set-integrity-level object "sealed")
	object)))

;;; This function should be implemented with underlying class replacement.
(defun -object.set-prototype-of (object prototype)
  (when (not (or (eql (-type prototype) 'null-type)
		 (eql (-type prototype) 'object-type)))
    (error "Type error."))
  (when (not (eql (-type prototype) 'object-type))
    (return-from -object.set-prototype-of object))
  (when (eql (-set-prototype-of object prototype) :false)
    (error "Type error."))
  object)

;;; Prototype property methods are handled with 'this', so define them as
;;; methods.
(defmethod %has-own-property ((this -object-proto) value)
  (let ((p (-to-property-key value))
	(o (-to-object this)))
    (if (eql (-get-own-property o p) :undefined)
	*boolean-false*
	*boolean-true*)))

(defmethod %is-prototype-of ((this -object-proto) value)
  (if (not (eql (-type this) 'object-type))
      *boolean-false*
      (loop for value = (-get-prototype-of value)
	 if (eql value :null)
	 do (return-from %is-prototype-of *boolean-false*)
	 ;; XXX: What does the standard mean? The instance itself as a
	 ;; prototype or the class object? -- Augustus, 18 Sep 2015.
	 if (eql value (class-of this))
	 do (return-from %is-prototype-of *boolean-true*))))

(defmethod %property-is-enumerable ((this -object-proto) value)
  (let* ((p (-to-property-key value))
	 (o (-to-object this))
	 (desc (-get-own-property o p)))
    (if (eql desc :undefined)
	*boolean-false*
	(if (eql (property-enumerable desc) :false)
	    *boolean-false*
	    *boolean-true*))))

(defmethod %to-locale-string ((this -object-proto))
  (%to-string this))

;;; I don't know what does the standard mean!
(defmethod %to-string ((this -object-proto) &optional radix)
  (declare (ignore radix))
  (let ((tag ""))
    (cond ((-is-array this)
	   (setf tag "Array"))
	  ((eql (type-of this) '-string-proto)
	   (setf tag "String"))
	  ((eql (type-of this) '-function-proto)
	   (setf tag "Function"))
	  ((eql (type-of this) '-error-proto)
	   (setf tag "Error"))
	  ((eql (type-of this) '-boolean-proto)
	   (setf tag "Boolean"))
	  ((eql (type-of this) '-number-proto)
	   (setf tag "Number"))
	  ((eql (type-of this) '-date-proto)
	   (setf tag "Date"))
	  ((eql (type-of this) '-reg-exp-proto)
	   (setf tag "RegExp"))
	  (t
	   (let ((new-tag (-get this *symbol-to-string-tag*)))
	     (if (eql :undefined new-tag)
		 (setf tag "Object")
		 (setf tag new-tag)))))
    (!string (concatenate 'string "[object " tag "]"))))

(defmethod %value-of ((this -object-proto))
  (-to-object this))
