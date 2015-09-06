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

(defclass -array-buffer-prototype (-object-prototype)
  ((-prototype :initform '-object-prototype :allocation :class)
   (-array-buffer-data :type (or array -null) :initarg :-array-buffer-data)
   (-array-buffer-data-length :type integer :initarg :-array-buffer-data-length)
   (constructor :initform (make-property :value '-array-buffer)
		:allocation :class)
   (properties
    :initform
    (append '((byte-length . (make-property :get 'byte-length))
	      (slice . (make-property :value 'slice))
	      (to-string-tag . (make-property :value "ArrayBuffer"
				:configurable :true)))
	    (fetch-properties (find-class '-object-prototype)))
    :allocation :class))
  (:documentation ""))

(defclass -array-buffer (-function-prototype)
  ((-prototype :initform '-function-prototype :allocation :class)
   (length :initform (make-property :value 1) :allocation :class)
   (prototype :type property :allocation :class :initarg :prototype
	      :initform (make-property :value '-array-buffer-prototype))
   (properties
    :initform
    (append '((is-view . (make-property :value 'is-view))
	      (species . (make-property :value 'species)))
	    (fetch-properties (find-class '-function-prototype)))
    :allocation :class))
  (:documentation "Array buffer constructor, used only with new operator."))
