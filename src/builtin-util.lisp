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

;;;; Useful utilities.

;;; These two functions are called when some lisp-ish function wants to have
;;; it's name es-ish, or vice versa. Argument will be string, since symbol
;;; can't tell upper and lowercase.
(defun camel-to-hyphen (name)
  "Cast a camel style function name into a lisp style function name."
  (declare (type string name))
  (let ((result (make-array 0 :element-type 'character :fill-pointer t))
	;; To handle name like URIError, have to create a state.
	;; out of URI & NaN -> 0
	;; U|RI -> 1
	;; UR|I -> 2
	;; To handle NaN...
	;; N|aN -> 3
	;; Na|N -> 4
	(uri-nan-state 0))
    (loop for char across (the string name) do
       ;; Only meeting with those upcase-able char in their uppercase form
       ;; do we need to add a hyphen and cast it to lowercase.
	 (if (char= char #\_)
	     (vector-push-extend #\- result)
	     (if (char= (char-downcase char) char)
		 (if (and (= uri-nan-state 3) (char= char #\a))
		     (progn
		       (setf uri-nan-state 4)
		       (vector-push-extend #\a result))
		     (progn
		       (setf uri-nan-state 0)
		       (vector-push-extend char result)))
		 (progn
		   (setf uri-nan-state 0)
		   (vector-push-extend char result))
		 (cond ((and (= uri-nan-state 0) (char= char #\U))
			(progn
			  (setf uri-nan-state 1)
			  (vector-push-extend #\- result)
			  (vector-push-extend #\u result)))
		       ((and (= uri-nan-state 1) (char= char #\R))
			(progn
			  (setf uri-nan-state 2)
			  (vector-push-extend #\r result)))
    result))))))

;;; FIXME: Make these two functions applied as an identity,
;;; handle URI and NaN correctly.
(defun hyphen-to-camel (name)
  "Cast a lisp style function name into a camel style function name."
  (declare (type string name))
  (let ((1st (char name 0))
	;; Transform -this-is-an-example into ThisIsAnExample.
	(result (remove #\- (string-capitalize name))))
    (if (char= 1st #\-)
	(setf result (string-left-trim "-" result))
	(setf result (string-downcase result :end 1)))
    result))

;;; To get the owned and inherited property list fast, use this method.
;;; FIXME: Another better one?
(defgeneric fetch-properties (this)
  (:documentation "Fetch the PROPERTIES slot from a class object, create
an instance of the class object and fetch its slot."))

