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

(defclass -string-prototype (-object-prototype)
  ((-prototype :initform '-object-prototype)
   (-string-data :type string-raw :initarg :-string-data)
   (length :type property :initarg :length :initform (make-property :value 0))
   (constructor :initform '-string :allocation :class)
   (properties
    :initform
    (append (fetch-properties (find-class '-object-prototype))
	    '((char-at . (make-property :value 'char-at))
	      (char-code-at . (make-property :value 'char-code-at))
	      (code-point-at . (make-property :value 'code-point-at))
	      (concat . (make-property :value 'concat))
	      (ends-with . (make-property :value 'ends-with))
	      (includes . (make-property :value 'includes))
	      (index-of . (make-property :value 'index-of))
	      (last-index-of . (make-property :value 'last-index-of))
	      (locale-compare . (make-property :value 'locale-compare))
	      (match . (make-property :value 'match))
	      (normalize . (make-property :value 'normalize))
	      (repeat . (make-property :value 'repeat))
	      (replace . (make-property :value 'replace))
	      (search . (make-property :value 'search))
	      (slice . (make-property :value 'slice))
	      (split . (make-property :value 'split))
	      (starts-with . (make-property :value 'starts-with))
	      (substring . (make-property :value 'substring))
	      (to-locale-lower-case . (make-property :value 'to-locale-lower-case))
	      (to-locale-upper-case . (make-property :value 'to-locale-upper-case))
	      (to-lower-case . (make-property :value 'to-lower-case))
	      (to-upper-case . (make-property :value 'to-upper-case))
	      (trim . (make-property :value 'trim))
	      (iterator . (make-property :value 'iterator))))
    :allocation :class))
  (:documentation "String prototype, provides inherited properties."))

(defclass -string (-function-prototype)
  ((-prototype :initform '-function-prototype)
   (length :initform (make-property :value 1) :allocation :class)
   (prototype :type (or property -null) :allocation :class :accessor prototype
	      :initarg :prototype
	      :initform (make-property :value '-string-prototype))
   (properties
    :initform
    (append (fetch-properties (find-class '-function-prototype))
	    '((from-char-code . (make-property :value 'from-char-code))
	      (from-code-point . (make-property :value 'from-code-point))
	      (raw . (make-property :value 'raw))))
    :allocation :class))
  (:documentation "String constructor, used with new operator."))

(defclass -string-iterator-prototype (-iterator-prototype)
  ((-prototype :initform '-iterator-prototype)
   (-iterated-string :type -string-prototype :initarg :-iterated-string)
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

(defmethod fetch-properties ((this -string-prototype))
  (properties (make-instance (class-name this))))

(defmethod fetch-properties ((this -string))
  (properties (make-instance (class-name this))))

(defun from-char-code (&rest code-units)
  )

(defun from-code-point (&rest code-points)
  )

(defun raw (template &rest substitutions)
  )

(defmethod to-string ((this -string-prototype))
  )

(defmethod to-locale-string ((this -string-prototype))
  )

(defmethod value-of ((this -string-prototype))
  )
