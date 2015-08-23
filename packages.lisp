(in-package :cl-user)

(defpackage :jsimple-error
  (:use :cl)
  (:documentation
   "Error module of jsimple.")
  (:export :general-error
	   :lexer-error
	   :parser-error
	   :runtime-error
	   :arithmetic-error
	   :divide-by-0-error
	   :invalid-index
	   :invalid-property
	   :invalid-function
	   :invalid-object
	   :number-too-large))

(defpackage :jsimple-parser
  (:use :cl
	:jsimple-error)
  (:documentation
   "Parser of jsimple.")
  (:export :*line*
	   :*char*
	   :*position*))

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
