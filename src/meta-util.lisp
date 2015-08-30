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

;;;; Utilities of metaclass.
(in-package :jsimple-builtin)

(declaim (inline delistify))
(defun delistify (list)
  (if (and (listp list) (null (cdr list)))
      (car list)
      list))

(defun remove-keyword-arg (arglist akey)
  (let ((lst arglist)
	(out ()))
    (labels ((pop-arg (alist)
	       (let ((arg (pop alist))
		     (val (pop alist)))
		 (unless (equal arg akey)
		   (setf out (append (list arg val) out)))
		 (when alist (pop-arg alist)))))
      (pop-arg lst))
    out))

(defun insert-before (new old list)
  (labels ((build-list (old c &optional newlist)
	     (if c
		 (if (eq old (car c))
		     (append (reverse (cdr c))
			     (cons (car c) (cons new newlist)))
		     (build-list old (cdr c) (cons (car c) newlist)))
		 (cons new newlist))))
    (reverse (build-list old list))))

(defun fill-slot-from-ancestor (slot class)
  (let ((ancestor (find-if #'(lambda (an)
			       (when (slot-exists-p an slot)
				 (slot-boundp an slot)))
			   (cdr (compute-class-precedence-list class)))))
    (when ancestor
      (setf (slot-value class slot) (slot-value ancestor slot)))))
