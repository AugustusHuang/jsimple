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

(defclass -set-prototype (-object-prototype)
  ((-prototype :initform '-object-prototype)
   (-set-data :type list :initarg :-set-data)
   (constructor :initform (make-property :value '-function-prototype)
		:allocation :class)
   (properties
    :initform
    (append '((add . (make-property :value 'add))
	      (clear . (make-property :value 'clear))
	      (delete . (make-property :value '!delete))
	      (entries . (make-property :value 'entries))
	      (for-each . (make-property :value 'for-each))
	      (has . (make-property :value 'has))
	      (keys . (make-property :value 'keys))
	      (size . (make-property :get 'size))
	      (values . (make-property :value '!values))
	      (iterator . (make-property :value 'iterator))
	      (to-string-tag . (make-property :value "Set"
				:configurable :true))))
	    (fetch-properties (find-class '-object-prototype))
    :allocation :class)
  (:documentation "Set prototype, provides inherited properties."))

(defclass -set (-function-prototype)
  ((-prototype :initform '-function-prototype)
   (length :initform (make-property :value 0) :allocation :class)
   (prototype :type (or property -null) :allocation :class :initarg :prototype
	      :initform (make-property :value '-set-prototype))
   (properties
    :initform
    (append '((species . (make-property :value 'species)))
	    (fetch-properties (find-class '-function-prototype)))
    :allocation :class))
  (:documentation "Set constructor, used with new operator."))

(defclass -set-iterator-prototype (-iterator-prototype)
  ((-prototype :initform '-iterator-prototype)
   (-iterated-set :type -set-prototype :initarg :-iterated-set)
   (-set-next-index :type integer :initarg :-set-next-index)
   ;; "key", "value" or "key+value", where "key" and "value" means the same.
   (-set-iteration-kind :type string :initarg :-set-iteration-kind)
   (properties
    :initform
    (append '((to-string-tag . (make-property :value "Set Iterator")))
	    (fetch-properties (find-class '-iterator-prototype)))
    :allocation :class))
  (:documentation "Set iterator prototype, provides inherited set iterator
objects properties."))

(defclass -weak-set-prototype (-object-prototype)
  ((-prototype :initform '-object-prototype)
   (-weak-set-data :type hash-table :initarg :-weak-set-data)
   (constructor :initform '-weak-set :allocation :class)
   (properties
    :initform
    (append '((add . (make-property :value 'add))
	      (delete . (make-property :value '!delete))
	      (has . (make-property :value 'has))
	      (to-string-tag . (make-property :value "WeakSet"
				:configurable :true)))
	    (fetch-properties (find-class '-object-prototype)))
    :allocation :class)
  (:documentation "Weak set prototype, provides inherited properties."))

(defclass -weak-set (-function-prototype)
  ((-prototype :initform '-function-prototype)
   (length :initform (make-property :value 0) :allocation :class)
   (prototype :type (or property -null) :allocation :class :initarg :prototype
	      :initform (make-property :value '-weak-set-prototype)))
  (:documentation "Weak set constructor, used with new operator."))

(defmethod fetch-properties ((this -set-prototype))
  (properties (make-instance (class-name this))))

(defmethod fetch-properties ((this -set))
  (properties (make-instance (class-name this))))

(defmethod fetch-properties ((this -set-iterator-prototype))
  (properties (make-instance (class-name this))))

(defmethod fetch-properties ((this -weak-set-prototype))
  (properties (make-instance (class-name this))))

(defmethod fetch-properties ((this -weak-set))
  (properties (make-instance (class-name this))))

