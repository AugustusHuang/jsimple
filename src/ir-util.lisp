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
(defmacro ir-translate (operator (operations entry next exit) &body body)
  )

;;; Every js form will have its own translate function, made by IR-TRANSLATE.
;;; BODY will contain specific operations needed to complete a translation.
(ir-translate block ((&rest operations) entry next exit)
	      )

(ir-translate if ((test then &rest else) entry next exit)
	      )

(ir-translate function
	      )

(ir-translate binary
	      )

(ir-translate unary
	      )

(ir-translate let
	      )

(ir-translate var
	      )

(ir-translate const
	      )

(ir-translate switch
	      )

(ir-translate catch
	      )

(ir-translate throw
	      )

(ir-translate class
	      )

(ir-translate array
	      )

(ir-translate object
	      )

(ir-translate string
	      )

