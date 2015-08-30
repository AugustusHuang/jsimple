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

;;;; asdf file

(in-package :asdf-user)

(defsystem :jsimple
  :version "0.0.1"
  :licence "MIT"
  :description "A javascript interpreter in Common Lisp to make running javascript simple."
  :components
  ((:module
    :src
    :components
    ((:cl-source-file "packages")
     (:cl-source-file "error" :depends-on ("packages"))
     (:cl-source-file "lexer" :depends-on ("packages" "error"))
     (:cl-source-file "parser" :depends-on ("packages" "error" "lexer"))
;     (:cl-source-file "meta-util" :depends-on ("packages"))
;     (:cl-source-file "array" :depends-on ("packages" "error" "meta-util"))
;     (:cl-source-file "string" :depends-on ("packages" "error" "meta-util"))
;     (:cl-source-file "object" :depends-on ("packages" "error" "meta-util"))
;     (:cl-source-file "builtin" :depends-on ("packages" "error" "array"
;							"string" "object"))
;     (:cl-source-file "ir-trans" :depends-on ("packages" "error" "builtin"))
;     (:cl-source-file "ir-util" :depends-on ("packages" "error" "parser"))
;     (:cl-source-file "node" :depends-on ("packages" "error" "parser"))
;     (:cl-source-file "piece" :depends-on ("packages" "error" "parser" "node"))
;     (:cl-source-file "ir-opt" :depends-on ("packages" "error" "node" "piece"))
     ))
   (:static-file "LICENSE")
   (:static-file "README.md")
   (:static-file "HACKING.md")
   (:static-file "IR.md")))
