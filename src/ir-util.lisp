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

;;;; Utilities used in IR transform and optimize stage.
(in-package :jsimple-ir)

;;; The general macro to translate a form with CAR in js content into
;;; a node or methods applied to nodes.

;;; This is the core function to translate a form into a node,
;;; so what's the differences?
(declaim (ftype (function (ctran ctran (or lvar null) t) (values))
                ir1-convert))
(macrolet (;; Bind *COMPILER-ERROR-BAILOUT* to a function that throws
           ;; out of the body and converts a condition signalling form
           ;; instead. The source form is converted to a string since it
           ;; may contain arbitrary non-externalizable objects.
           (ir1-error-bailout ((start next result form) &body body)
             (with-unique-names (skip condition)
               `(block ,skip
                  (let ((,condition (catch 'ir1-error-abort
                                      (let ((*compiler-error-bailout*
                                              (lambda (&optional e)
                                                (throw 'ir1-error-abort e))))
                                        ,@body
                                        (return-from ,skip nil)))))
                    (ir1-convert ,start ,next ,result
                                 (make-compiler-error-form ,condition
                                                           ,form)))))))

  ;; Translate FORM into IR1. The code is inserted as the NEXT of the
  ;; CTRAN START. RESULT is the LVAR which receives the value of the
  ;; FORM to be translated. The translators call this function
  ;; recursively to translate their subnodes.
  ;;
  ;; As a special hack to make life easier in the compiler, a LEAF
  ;; IR1-converts into a reference to that LEAF structure. This allows
  ;; the creation using backquote of forms that contain leaf
  ;; references, without having to introduce dummy names into the
  ;; namespace.
  (defun ir1-convert (start next result form)
    (let* ((*current-path* (ensure-source-path form))
           (start (instrument-coverage start nil form)))
      (ir1-error-bailout (start next result form)
        (cond ((atom form)
               (cond ((and (symbolp form) (not (keywordp form)))
                      (ir1-convert-var start next result form))
                     ((leaf-p form)
                      (reference-leaf start next result form))
                     (t
                      (reference-constant start next result form))))
              (t
               (ir1-convert-functoid start next result form)))))
    (values)))

(defmacro ir-translate (operator (operations start next end) &body body)
  ;; Here OPERATOR should be a keyword, so we will get the proper name.
  (let ((name (concatenate 'string
			   "ir-translate-"
			   (string-downcase (symbol-name operator))))
	(whole (make-symbol "form")))
    `(progn
       (declaim (ftype (function (ctran ctran (or lvar null) t) (values))
		       ,name))
       (defun ,name (,start ,next ,end ,whole)
	 (declare (ignore ,start ,next ,end))
	 ;; How to implement this?
	 ))))

;;; Every js form will have its own translate function, made by IR-TRANSLATE.
;;; BODY will contain specific operations needed to complete a translation.
;;; Since there won't be any keyword arguments, keyword operator won't bring
;;; any trouble.
(ir-translate :block ((&rest operations) start next end)
	      )

(ir-translate :if ((test then &rest else) start next end)
	      )

(ir-translate :defun
	      )

(ir-translate :stat
	      )

(ir-translate :assign
	      )

(ir-translate :let
	      )

(ir-translate :var
	      )

(ir-translate :const
	      )

(ir-translate :switch
	      )

(ir-translate :catch
	      )

(ir-translate :try
	      )

(ir-translate :throw
	      )

(ir-translate :class
	      )

(ir-translate :array
	      )

(ir-translate :object
	      )

(ir-translate :string
	      )

(ir-translate :for
	      )

(ir-translate :return
	      )

(ir-translate :with
	      )
