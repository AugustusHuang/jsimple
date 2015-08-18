(in-package :cl-user)

(defpackage :jsimple-error
  (:use :cl)
  (:documentation
   "Error module of jsimple.")
  (:export :general-error
	   :lexer-error
	   :parser-error
	   :runtime-error))

(defpackage :jsimple-parser
  (:use :cl
	:jsimple-error
	:yacc)
  (:documentation
   "Parser of jsimple, a simple javascript interpreter written in Common Lisp.")
  (:export))

(defpackage :jsimple-runtime
  (:use :cl
	:jsimple-error
	:jsimple-parser)
  (:documentation
   "Runtime environment of jsimple.")
  (:export))

(defpackage :jsimple-test
  (:use :cl
	:jsimple-error
	:jsimple-parser
	:jsimple-runtime)
  (:documentation
   "Testsuite of jsimple.")
  (:export))
