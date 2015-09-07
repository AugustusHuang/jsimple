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

(defclass -promise-prototype (-object-prototype)
  ((-prototype :initform '-object-prototype)
   ;; "pending", "fulfilled" or "rejected".
   (-promise-state :type string :initarg :-promise-state)
   ;; Only meaningful if [[PromiseState]] is not "pending".
   (-promise-result :initarg :-promise-result)
   ;; A list of PromiseReaction records when the promise transitions
   ;; from "pending" to "fulfilled".
   (-promise-fulfill-reactions :initarg :-promise-fulfill-reactions)
   ;; A list of PromiseReaction records when the promise transitions
   ;; from "pending" to "rejected".
   (-promise-reject-reactions :initarg :-promise-reject-reactions)
   (constructor :initform (make-property :value '-promise) :allocation :class)
   (properties
    :initform
    (append '((catch . (make-property :value '!catch))
	      (then . (make-property :value 'then))
	      (to-string-tag . (make-property :value "Promise"
				:configurable :true)))
	    (fetch-properties (find-class '-object-prototype)))
    :allocation :class))
  (:documentation "Promise prototype, provides inherited properties."))

(defclass -promise (-function-prototype)
  ((-prototype :initform '-function-prototype)
   (length :initform (make-property :value 1) :allocation :class)
   (prototype :type (or property -null) :allocation :class :initarg :prototype
	      :initform (make-property :value '-promise-prototype))
   (properties
    :initform
    ;; ALL has a resolve element function, which is an anonymous built-in
    ;; used to resolve a specific ALL element, has [[Index]], [[Values]],
    ;; [[Capabilities]], [[RemainingElements]] and [[AlreadyCalled]] slots.
    (append '((all . (make-property :value 'all))
	      (race . (make-property :value 'race))
	      (reject . (make-property :value 'reject))
	      (resolve . (make-property :value 'resolve))
	      (species . (make-property :value 'species)))
	    (fetch-properties (find-class '-function-prototype)))
    :allocation :class))
  (:documentation "Promise constructor, used only with new operator."))

(defmethod fetch-properties ((this -promise-prototype))
  (properties (make-instance (class-name this))))

(defmethod fetch-properties ((this -promise))
  (properties (make-instance (class-name this))))
