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
  ((constructor :reader constructor :type string
		:initarg :constructor :initform "Object"
		:allocation class)
   ;; Keys will be strings or symbols, values will be DATA-PROPERTY or
   ;; ACCESSOR-PROPERTY.
   (properties :accessor properties :type (or list null)
	       :initarg :properties :initform nil))
  (:documentation "Builtin object prototype."))

(defun object-properties (object)
  (slot-value object 'properties))

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

(defmethod print-object ((this -object) stream)
  ;; ALIST looks like ((a . b) (c . d) (e . f)).
  ;; Make it into form ((a b) (c d) (e f)). Sequence doesn't matter.
  (labels ((pair-out (lst)
	     (let ((result ()))
	       (loop for pair in lst do
		    (push (list (car pair) (cdr pair)) result))
	       result)))
    (format stream "<Object:~:{ <~S: ~S>~}>"
	    (pair-out (object-properties object)))
    this))

(defun -object-constructor (&optional value)
  (to-object value))

(defun to-object (value)
  (typecase value
    (-undefined
     )
    (-null
     )
    (-boolean
     )
    (-number
     )
    (-string
     )
    (-symbol
     )))
