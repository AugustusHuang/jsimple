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

;;; Object related builtin functions.
(defclass -object ()
  ((constructor :reader constructor :type function
		:initarg :constructor :initform #'-new-object
		:allocation class)
   ;; Keys will be strings or symbols, values will be DATA-PROPERTY or
   ;; ACCESSOR-PROPERTY.
   (properties :accessor properties :type (or list null)
	       :initarg :properties :initform nil))
  (:documentation "Builtin object prototype."))

;;; In ECMA-262, internal slots are not object properties and they are not
;;; inherited, and they are allocated as part of the process of creating an
;;; object and may not be dynamically added to an object.
;;; So choose mixin as the specific instance created in a object way.
;;; It is misleading, but we don't need the slots provided by specific class
;;; or they will be redundant, when we need to cast the type, fetch the value.
(defclass -object-boolean (-object)
  ((boolean-data :initarg :data :type boolean-raw :initform :false))
  (:documentation "Object type constructed by new Boolean()."))

(defclass -object-number (-object)
  ((number-data :initarg :data :type number-raw :initform 0))
  (:documentation "Object type constructed by new Number()."))

(defclass -object-string (-object)
  ((string-data :initarg :data :type string-raw :initform ""))
  (:documentation "Object type constructed by new String()."))

(defclass -object-symbol (-object)
  ((symbol-data :initarg :data :type symbol-raw))
  (:documentation "Object type constructed by new Symbol()."))

;;; VALUE is a form ((KEY1 :TYPE1 VALUE1) (KEY2 :TYPE2 VALUE2)).
(defun -new-object (&optional value)
  (let ((assoc-list ()))
    (dolist (key-value value)
      (let* ((key (first key-value))
	     (value (third key-value))
	     (property (make-property :value value)))
	;; If we found a defined property, replace it with a new CDR.
	;; Else push a new property onto ASSOC-LIST.
	(if (cdr (assoc key assoc-list))
	    (rplacd (assoc key assoc-list) property)
	    (setf assoc-list (acons key property assoc-list)))))
    (make-instance '-object :properties assoc-list)))

(defun -new-object-boolean (value)
  )

(defun -new-object-number (value)
  )

(defun -new-object-string (value)
  )

(defun -new-object-symbol (value)
  )

;;; We need PRINT-OBJECT methods for all mixins and original object type.
;;; Object style: <Object: <a: 1> <b: 2>>
;;; Boolean object style: <Boolean: <primitive-value: true>>
;;; Number object style: <Number: <primitive-value: 3>>
;;; String object style:
;;; <String: <0: "s"> <1: "t"> <2: "r"> <length: 3> <primitive-value: "str">>
;;; Symbol object style: <Symbol: <primitive-value: 'sym>>.
(defmethod print-object ((this -object) stream)
  ;; ALIST looks like ((a . b) (c . d) (e . f)).
  ;; Make it into form ((a b) (c d) (e f)).
  (labels ((pair-out (lst)
	     (let ((result ()))
	       (loop for pair in lst do
		    (push (list (car pair) (cdr pair)) result))
	       result)))
    (format stream "<Object:~:{ <~S: ~S>~}>"
	    (pair-out (properties object))))
  this)

(defmethod print-object ((this -object-boolean) stream)
  (format stream "<Boolean: <boolean-data: ~A>"
	  (slot-value this 'boolean-data))
  this)

(defmethod print-object ((this -object-number) stream)
  (format stream "<Number: <number-data: ~A>"
	  (slot-value this 'number-data))
  this)

(defmethod print-object ((this -object-string) stream)
  (labels ((pair-out (lst)
	     (let ((result ()))
	       (loop for pair in lst do
		    (push (list (car pair) (cdr pair)) result))
	       result)))
    (format stream "<String:~:{ <~S: ~S>~} <string-data: ~A>>"
	    (pair-out (properties this))
	    (slot-value this 'string-data)))
  this)

(defmethod print-object ((this -object-symbol) stream)
  (format stream "<Symbol: <symbol-data: ~A>"
	  (slot-value this 'symbol-data))
  this)

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
