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

(defclass -map-prototype (-object-prototype)
  ((-prototype :initform '-object-prototype :allocation :class)
   (-map-data :type list :initarg :-map-data)
   (constructor :initform (make-property :value '-map))
   (properties
    :initform
    (append (fetch-properties (find-class '-object-prototype))
	    '((clear . (make-property :value 'clear))
	      (delete . (make-property :value '!delete))
	      (entries . (make-property :value 'entries))
	      (for-each . (make-property :value 'for-each))
	      (get . (make-property :value '!get))
	      (has . (make-property :value 'has))
	      (keys . (make-property :value 'keys))
	      (set . (make-property :value '!set))
	      (size . (make-property :get 'size))
	      (values . (make-property :value 'values))
	      (iterator . (make-property :value 'iterator))
	      (to-string-tag . (make-property :value "Map"))))
    :allocation :class)
  (:documentation "Map prototype, provides inherited properties."))

(defclass -map (-function-prototype)
  ((-prototype :initform '-function-prototype :allocation :class)
   (length :initform (make-property :value 0) :allocation :class)
   (prototype :type (or property -null) :allocation :class :initarg :prototype
	      :initform (make-property :value '-map-prototype))
   (properties
    :initform
    (append (fetch-properties (find-class '-function-prototype))
	    '((species . (make-property :value 'species))))
    :allocation :class))
  (:documentation "Map constructor, used with new operator."))

(defclass -map-iterator-prototype (-iterator-prototype)
  ((-prototype :initform '-iterator-prototype :allocation :class)
   (-map :type -map-prototype :initarg :-map)
   (-map-next-index :type integer :initarg :-map-next-index)
   ;; "key" "value" or "key+value".
   (-map-iteration-kind :type string :initarg :-map-iteration-kind)
   (properties
    :initform
    (append '((to-string-tag . (make-property :value "Map Iterator")))
	    (fetch-properties (find-class '-iterator-prototype)))
    :allocation :class))
  (:documentation "Map iterator prototype, provides inherited map iterator
object properties."))

(defclass -weak-map-prototype (-object-prototype)
  ((-prototype :initform '-object-prototype :allocation :class)
   (-weak-map-data :type hash-table :initarg :-weak-map-data)
   (constructor :initform '-weak-map :allocation :class)
   (properties
    :initform
    (append '((delete . (make-property :value '!delete))
	      (get . (make-property :value '!get))
	      (has . (make-property :value 'has))
	      (set . (make-property :value '!set))
	      (to-string-tag . (make-property :value "WeakMap"
				:configurable :true))))
    :allocation :class))
  (:documentation "Weak map prototype, provides inherited properties."))

(defclass -weak-map (-function-prototype)
  ((-prototype :initform '-function-prototype :allocation :class)
   (length :initform (make-property :value 0) :allocation :class)
   (prototype :type property :allocation :class :initarg :prototype
	      :initform (make-property :value '-weak-map-prototype)))
  (:documentation "Weak constructor, used with new operator."))

(defmethod fetch-properties ((this -map-prototype))
  (properties (make-instance (class-name this))))

(defmethod fetch-properties ((this -map))
  (properties (make-instance (class-name this))))

(defmethod fetch-properties ((this -map-iterator-prototype))
  (properties (make-instance (class-name this))))

(defmethod fetch-properties ((this -weak-map-prototype))
  (properties (make-instance (class-name this))))

(defmethod fetch-properties ((this -weak-map))
  (properties (make-instance (class-name this))))
