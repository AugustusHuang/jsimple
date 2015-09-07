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

(defclass -reflect (-object-prototype)
  ((-prototype :initform '-object-prototype)
   (properties
    :initform
    (append '((apply . (make-property :value '!apply))
	      (construct . (make-property :value 'construct))
	      (define-property . (make-property :value 'define-property))
	      (delete-property . (make-property :value 'delete-property))
	      (enumerate . (make-property :value 'enumerate))
	      (get . (make-property :value '!get))
	      (get-own-property-descriptor . (make-property :value 'get-own-property-descriptor))
	      (get-prototype-of . (make-property :value 'get-prototype-of))
	      (has . (make-property :value 'has))
	      (is-extensible . (make-property :value 'is-extensible))
	      (own-keys . (make-property :value 'own-keys))
	      (prevent-extensions . (make-property :value 'prevent-extesions))
	      (set . (make-property :value '!set))
	      (set-prototype-of . (make-property :value 'set-prototype-of)))
	    (fetch-properties (find-class '-object-prototype)))
    :allocation :class))
  (:documentation "Reflect object, provides initial value of reflect property."))

(defmethod fetch-properties ((this -reflect))
  (properties (make-instance (class-name this))))
