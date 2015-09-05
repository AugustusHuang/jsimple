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

;;;; Builtin symbol type definitions.
(in-package :jsimple-builtin)

(deftype symbol-raw ()
  'symbol)

;;; Well known symbols are built-in symbol values are typically used
;;; as the keys of properties. -- ECMA V6.
(defclass -symbol-prototype (-object-prototype)
  ((-prototype :initform '-object-prototype :allocation :class)
   (-symbol-data :type symbol-raw :initarg :-symbol-data)
   (constructor :initform (make-property :value '-symbol) :allocation :class)
   (own-properties
    :initform '((to-primitive . (make-property :value 'to-primitive
				 :configurable :true))
		(to-string-tag . (make-property :value "Symbol"
				  :configurable :true)))
    :allocation :class)
   (inherit-properties
    :initform (append (fetch-own-properties (find-class '-object-prototype))
		      (fetch-inherit-properties (find-class '-object-prototype)))
    :allocation :class))
  (:documentation "Symbol prototype, provides inherited properties."))

(defclass -symbol (-function-prototype)
  ((-prototype :initform '-function-prototype :allocation :class)
   (-extensible :initform :false :allocation :class)
   (length :initform (make-property :value 0) :allocation :class)
   (prototype :type (or property -null) :accessor prototype
	      :initarg :prototype :initform (make-property :value '-symbol-prototype)
	      :allocation :class)
   (own-properties
    :initform '((for . (make-property :value 'key))
		(has-instance . (make-property :value 'has-instance))
		(is-concat-spreadable . (make-property :value 'is-concat-spreadable))
		(iterator . (make-property :value 'iterator))
		(key-for . (make-property :value 'key-for))
		(match . (make-property :value 'match))
		(replace . (make-property :value 'replace))
		(search . (make-property :value 'search))
		(species . (make-property :value 'species))
		(split . (make-property :value 'split))
		(to-primitive . (make-property :value 'to-primitive))
		(to-string-tag . (make-property :value 'to-string-tag))
		(unscopables . (make-property :value 'unscopables)))
    :allocation :class)
   (inherit-properties
    :initform (append (fetch-own-properties (find-class '-function-prototype))
		      (fetch-inherit-properties (find-class '-function-prototype)))
    :allocation :class))
  (:documentation "Symbol constructor, used with new operator."))

(defmethod print-object ((this -symbol-prototype) stream)
  )

(defmethod to-string ((this -symbol-prototype))
  )

(defmethod value-of ((this -symbol-prototype))
  )

(defmethod to-primitive ((this -symbol-prototype) hint)
  )

