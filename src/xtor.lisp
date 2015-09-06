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

;;;; All iterator function, generator function definitions.
(in-package :lesp-builtin)

(defclass -iterator-prototype (-object-prototype)
  ((-prototype :initform '-object-prototype :allocation :class)
   (properties
    :initform
    (append '((iterator . (make-property :value 'iterator)))
	    (fetch-properties (find-class '-object-prototype)))
    :allocation :class))
  (:documentation "Iterator prototype, provides inherited properties."))

(defclass -generator-prototype (-iterator-prototype)
  ((-prototype :initform '-iterator-prototype :allocation :class)
   ;; :UNDEFINED, "suspendedStart" "suspendedYield" "executing" or "completed".
   (-generator-state :type (or string -undefined) :initarg :-generator-state)
   (-generator-context :initarg :-generator-context)
   (constructor :initform (make-property :value '-generator
					 :configurable :true))
   (properties
    :initform
    (append '((next . (make-property :value 'next))
	      (return . (make-property :value '!return))
	      (throw . (make-property :value '!throw))
	      (to-string-tag . (make-property :value "Generator"
				:configurable :true)))
	    (fetch-properties (find-class '-iterator-prototype)))
    :allocation :class))
  (:documentation "Generator prototype, provides indirect inherited properties."))

(defclass -generator (-function-prototype)
  ((-prototype :initform '-function-prototype :allocation :class)
   (-function-kind :initform "generator")
   ;; Configurable: TRUE.
   ;; Length and name are the same as FunctionPrototype.
   (constructor :initform (make-property :value '-generator-function
					 :configurable :true)
		:allocation :class :initarg :constructor)
   ;; Every generator will have a prototype to become the generator function's
   ;; [[Prototype]].
   (prototype :type (or property -null) :accessor prototype
	      :initarg :prototype
	      ;; Once changed, the property has writable: TRUE.
	      :initform (make-property :value '-generator-prototype
				       :configurable :true))
   (properties
    :initform
    (append '((to-string-tag . (make-property :value "GeneratorFunction"
				:configurable :true)))
	    (fetch-properties (find-class '-function-prototype)))
    :allocation :class))
  (:documentation "Generator function."))

(defclass -generator-function (-function)
  ((-prototype :initform '-function :allocation :class)
   (length :initform (make-property :value 1
				    :configurable :true) :allocation :class)
   (name :initform (make-property :value "GeneratorFunction")
	 :type string :allocation :class)
   (prototype :initform '-generator :allocation :class)
   (properties
    :initform (fetch-properties (find-class '-function))
    :allocation :class))
  (:documentation "Generator function constructor, used with or without new
operator."))

(defmethod iterator ((this -iterator-prototype))
  "[Symbol.iterator]"
  this)

(defmethod fetch-properties ((this -iterator-prototype))
  (properties (make-instance (class-name this))))

(defmethod fetch-properties ((this -generator-prototype))
  (properties (make-instance (class-name this))))

(defmethod fetch-properties ((this -generator))
  (properties (make-instance (class-name this))))

(defmethod fetch-properties ((this -generator-function))
  (properties (make-instance (class-name this))))
