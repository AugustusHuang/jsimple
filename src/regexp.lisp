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

;;;; Builtin regular expression type definitions.
(in-package :lesp-builtin)

(defclass -reg-exp-prototype (-object-prototype)
  ((-prototype :initform '-object-prototype :allocation :class)
   (-reg-exp-matcher :type string :initarg :-reg-exp-matcher)
   (-original-source :type string :initarg :-original-source)
   (-original-flags :type string :initarg :-original-flags)
   (constructor :initform '-reg-exp :allocation :class)
   (last-index :type property :initarg :last-index
	       :initform (make-property :value 0 :writable :true))
   (properties
    :initform
    (append (fetch-properties (find-class '-object-prototype))
	    '((exec . (make-property :value 'exec))
	      (flags . (make-property :get 'flags))
	      (global . (make-property :get 'global))
	      (ignore-case . (make-property :get 'ignore-case))
	      (match . (make-property :value 'match))
	      (multiline . (make-property :get 'multiline))
	      (replace . (make-property :value 'replace))
	      (search . (make-property :value 'search))
	      (source . (make-property :get 'source))
	      (split . (make-property :value 'split))
	      (sticky . (make-property :get 'sticky))
	      (test . (make-property :value 'test))
	      (unicode . (make-property :get 'unicode))))
    :allocation :class))
  (:documentation "RegExp prototype, provides inherited properties."))

(defclass -reg-exp (-function-prototype)
  ((-prototype :initform '-function-prototype :allocation :class)
   (length :initform (make-property :value 2) :allocation :class)
   (prototype :type (or property -null) :allocation :class :initarg :prototype
	      :initform '-reg-exp-prototype)
   (properties
    :initform
    (append (fetch-properties (find-class '-function-prototype))
	    ;; Where is this function?
	    '((get . (make-property :value 'get))))
    :allocation :class))
  (:documentation "RegExp constructor, used with new operator."))

(defmethod fetch-properties ((this -reg-exp-prototype))
  (properties (make-instance (class-name this))))

(defmethod fetch-properties ((this -reg-exp))
  (properties (make-instance (class-name this))))
