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

;;;; Builtin string type definitions.
(in-package :lesp-builtin)

(defclass -string-proto (-object-proto)
  ((-prototype :initform (find-class '-object-prototype))
   (-string-data :type string-raw :initarg :-string-data)
   (length :type property :initarg :length :initform (make-property :value 0))
   (constructor :initform (make-property :value -string) :allocation :class)
   (char-at :type property :allocation :class
	    :initform (make-property :value !char-at))
   (char-code-at :type property :allocation :class
		 :initform (make-property :value !char-code-at))
   (code-point-at :type property :allocation :class
		  :initform (make-property :value !code-point-at))
   (concat :type property :allocation :class
	   :initform (make-property :value !concat))
   (ends-with :type property :allocation :class
	      :initform (make-property :value !ends-with))
   (includes :type property :allocation :class
	     :initform (make-property :value !includes))
   (index-of :type property :allocation :class
	     :initform (make-property :value !index-of))
   (last-index-of :type property :allocation :class
		  :initform (make-property :value !last-index-of))
   (locale-compare :type property :allocation :class
		   :initform (make-property :value !locale-compare))
   (match :type property :allocation :class
	  :initform (make-property :value !match))
   (normalize :type property :allocation :class
	      :initform (make-property :value !normalize))
   (repeat :type property :allocation :class
	   :initform (make-property :value !repeat))
   (replace :type property :allocation :class
	    :initform (make-property :value !replace))
   (search :type property :allocation :class
	   :initform (make-property :value !search))
   (slice :type property :allocation :class
	  :initform (make-property :value !slice))
   (split :type property :allocation :class
	  :initform (make-property :value !split))
   (starts-with :type property :allocation :class
		:initform (make-property :value !starts-with))
   (substring :type property :allocation :class
	      :initform (make-property :value !substring))
   (to-locale-lower-case :type property :allocation :class
			 :initform (make-property :value !to-locale-lower-case))
   (to-locale-upper-case :type property :allocation :class
			 :initform (make-property :value !to-locale-upper-case))
   (to-lower-case :type property :allocation :class
		  :initform (make-property :value !to-lower-case))
   (to-upper-case :type property :allocation :class
		  :initform (make-property :value !to-upper-case))
   (trim :type property :allocation :class
	 :initform (make-property :value !trim))
   (iterator :type property :allocation :class
	     :initform (make-property :value !iterator)))
  (:documentation "String prototype, provides inherited properties."))

(defclass -string-iterator-proto (-iterator-proto)
  ((-prototype :initform '-iterator-proto)
   (-iterated-string :type -string-proto :initarg :-iterated-string)
   (-string-iterator-next-index :type integer
				:initarg :-string-iterator-next-index)
   (properties
    :initform
    (append (fetch-properties (find-class '-iterator-prototype))
	    '((next . (make-property :value 'next))
	      (to-string-tag . (make-property :value "String Iterator"
				:configurable :true))))
    :allocation :class))
  (:documentation "String iterator prototype, provides inherited properties to
all string iterator objects."))

(defmethod fetch-properties ((this -string-proto))
  (properties (make-instance (class-name this))))

;;; Internal methods.
(defmethod -get-prototype-of ((this -string-proto))
  )

(defmethod -set-prototype-of ((this -string-proto) proto)
  )

(defmethod -is-extensible ((this -string-proto))
  )

(defmethod -prevent-extensions ((this -string-proto))
  )

(defmethod -get-own-property ((this -string-proto) key)
  )

(defmethod -has-property ((this -string-proto) key)
  )

(defmethod -get ((this -string-proto) key receiver)
  )

(defmethod -set ((this -string-proto) key value receiver)
  )

(defmethod -delete ((this -string-proto) key)
  )

(defmethod -define-own-property ((this -string-proto) key descriptor)
  )

(defmethod -enumerate ((this -string-proto))
  )

(defmethod -own-property-keys ((this -string-proto))
  )

(defun from-char-code (&rest code-units)
  )

(defun from-code-point (&rest code-points)
  )

(defun raw (template &rest substitutions)
  )

(defmethod to-string ((this -string-proto))
  )

(defmethod to-locale-string ((this -string-proto))
  )

(defmethod value-of ((this -string-proto))
  )
