
;;;; Parser package.
(in-package :jsimple-parser)

;; Maybe useful, declare here!
(defvar *parser-position* 0)

;;; Since Lisp is written with prefix operators, we have to convert it into
;;; javascript-land style.
(defun infix-to-prefix (a op b)
  (list op a b))

;;; Similarly, a++ will be translated into (INCF A) with no optimization.
(defun suffix-to-prefix (a op)
  (list op a))

;;; Select the second argument.
(defun snd (f s t)
  (declare (ignore f t))
  s)

;;; Grammar:
;;; TODO:
(define-parser *jsimple-parser*
  (:start-symbol statements)
  (:terminals
   (number
    id
    + ++ - --
    * / %
    += -= *= /= %= = == === != !==
    > < >= <=
    |(| |)|))
  (:precedence ((:left * / %) (:left + -)))

  (statements
   (statements statement)
   expression)

  (statement
   while-statement
   iteration-statement
   )

  (expression
   (expression + expression #'infix-to-prefix)
   (expression - expression #'infix-to-prefix)
   (expression * expression #'infix-to-prefix)
   (expression / expression #'infix-to-prefix)
   (expression % expression #'infix-to-prefix)
   (expression == expression #'infix-to-prefix)
   (expression === expression #'infix-to-prefix)
   (expression != expression #'infix-to-prefix)
   (expression !== expression #'infix-to-prefix)
   (expression < expression #'infix-to-prefix)
   (expression <= expression #'infix-to-prefix)
   (expression > expression #'infix-to-prefix)
   (expression >= expression #'infix-to-prefix)
   (expression = expression #'infix-to-prefix)
   (expression += expression #'infix-to-prefix)
   (expression -= expression #'infix-to-prefix)
   (expression *= expression #'infix-to-prefix)
   (expression /= expression #'infix-to-prefix)
   (expression %= expression #'infix-to-prefix)
   (expression ++ #'suffix-to-prefix)
   (expression -- #'suffix-to-prefix)
   term)

  (term
   number
   id
   (- term)
   (|(| expression |)| #'snd)))

;;; Test function, read a valid javascript clip and return a prefix-ed
;;; Lispish-script.
(defun parse (code)
  (parse-with-lexer (jsimple-lexer code)
		    *jsimple-parser*))

;;;; NOTE:
;;;; Javascript syntax and grammar in ECMA-262:
;;;; | means OR, so char | will be \|, \ means EXCEPT-FOR, char \ will be \\,
;;;; [] will contain alternative choice (not primary, but tail),
;;;; :: means CAN-BE-VIEWED-AS, () will contain anything that is optional,
;;;; {} will contain descriptive scoping elements.
;;;; - without surrounding space means natural scope, e.g. 0-9 means 0 to 9.
;;;; all other characters evaluate to themselves.
;;;;
;;;; input-element-div ::
;;;;   whitespace | line-terminator | comment | common-token |
;;;;   div-punctuator |  right-brace-punctuator
;;;; whitespace ::
;;;;   TAB | VT | FF | SP | NBSP | ZWNBSP | USP
;;;; line-terminator ::
;;;;   LF | CR | LS | PS
;;;; comment ::
;;;;   multi-line-comment | single-line-comment
;;;; multi-line-comment ::
;;;;   /* (multi-line-comment-chars) */
;;;; multi-line-comment-chars ::
;;;;   multi-line-not-asterisk-char (multi-line-comment-chars) |
;;;;   * (post-asterisk-comment-chars)
;;;; post-asterisk-comment-chars ::
;;;;   multi-line-not-forward-slash-or-asterisk-char (multi-line-comment-chars) |
;;;;   * (post-asterisk-comment-chars)
;;;; multi-line-not-asterisk-char ::
;;;;   source-character \ *
;;;; multi-line-not-forward-slash-or-asterish-char ::
;;;;   source-character \ [* | /]
;;;; single-line-comment ::
;;;;   // (single-line-comment-chars)
;;;; single-line-comment-chars ::
;;;;   single-line-comment-char (single-line-comment-chars)
;;;; single-line-comment-char ::
;;;;   source-character \ line-terminator
;;;; common-token ::
;;;;   id | punctuator | number | string | template
;;;; identifier-name ::
;;;;   identifier-start | identifier-name identifier-part
;;;; identifier-start ::
;;;;   unicode-id-start | $ | _ | \\ unicode-escape-sequence
;;;; identifier-part ::
;;;;   unicode-id-continue | $ | _ | \\ unicode-escape-sequence | ZWNJ | ZWJ
;;;; unicode-id-start ::
;;;;   {any unicode with property ID_Start or Other_ID_Start}
;;;; unicode-id-continue ::
;;;;   {any unicode with property ID_Continue, Other_ID_Continue or
;;;;   Other_ID_Start}
;;;; reserved-word ::
;;;;   keyword | future-reserved-word | null-literal | boolean-literal
;;;; keyword ::
;;;;   break | do | in | typeof | case | else | instanceof | var | catch |
;;;;   export | new | void | class | extends | return | while | const |
;;;;   finally | super | with | continue | for | switch | yield | debugger |
;;;;   function | this | default | if | throw | delete | import | try
;;;; future-reserved-word ::
;;;;   enum | await
;;;; punctuator ::
;;;;   \{ | \} | \( | \) | \[ | \] | . | ; | , | < | > | <= | >= | == | != |
;;;;   === | !== | + | - | * | % | ++ | -- | << | >> | >>> | & | \| | ^ | ! |
;;;;   ~ | && | \|\| | ? | : | = | += | -= | *= | %= | <<= | >>= | >>>= | &= |
;;;;   |= | ^= | =>
;;;; div-punctuator ::
;;;;   / | /=
;;;; null-literal ::
;;;;   null
;;;; boolean-literal ::
;;;;   true | false
;;;; numeric-literal ::
;;;;   decimal-literal | binary-integer-literal | octal-integer-literal |
;;;;   hex-integer-literal
;;;; decimal-literal ::
;;;;   decimal-integer-literal . (decimal-digits) (exponent-part) |
;;;;   . decimal-digits (exponent-part) | decimal-integer-literal (exponent-part)
;;;; decimal-integer-literal ::
;;;;   0 | non-zero-digit (decimal-digits)
;;;; decimal-digits ::
;;;;   decimal-digit | decimal-digits decimal-digit
;;;; decimal-digit ::
;;;;   0-9
;;;; non-zero-digit ::
;;;;   1-9
;;;; exponent-part ::
;;;;   exponent-indicator signed-integer
;;;; exponent-indicator ::
;;;;   e | E
;;;; signed-integer ::
;;;;   decimal-digits | + decimal-digits | - decimal-digits
;;;; binary-integer-literal ::
;;;;   0b binary-digits | 0B binary-digits
;;;; binary-digits ::
;;;;   binary-digit | binary-digits binary-digit
;;;; binary-digit ::
;;;;   0 | 1
;;;; octal-integer-literal ::
;;;;   0o octal-digits | 0O octal-digits
;;;; octal-digits ::
;;;;   octal-digit | octal-digits octal-digit
;;;; octal-digit ::
;;;;   0-7
;;;; hex-integer-literal ::
;;;;   0x hex-digits | 0X hex-digits
;;;; hex-digits ::
;;;;   hex-digit | hex-digits hex-digit
;;;; hex-digit ::
;;;;   0-9 | a-f | A-F
;;;; string-literal ::
;;;;   " (double-string-characters) " | ' (single-string-characters) '
;;;; double-string-characters ::
;;;;   double-string-character (double-string-characters)
;;;; single-string-characters ::
;;;;   single-string-character (single-string-characters)
;;;; double-string-character ::
;;;;   source-character \ [" | \\ | line-terminator]
;;;;   \\ escape-sequence
;;;;   line-continuation
;;;; single-string-character ::
;;;;   source-character \ [' | \\ | line-terminator]
;;;;   \\ escape-sequence
;;;;   line-continuation
;;;; line-continuation ::
;;;;   \\ line-terminator-sequence
;;;; escape-sequence ::
;;;;   character-escape-sequence | 0 | hex-escape-sequence | unicode-escape-sequence
;;;; character-escape-sequence ::
;;;;   single-escape-character | non-escape-character
;;;; single-escape-character ::
;;;;   ' | " | \\ | b | f | n | r | t | v
;;;; non-escape-character ::
;;;;   source-character \ [escape-character | line-terminator]
;;;; escape-character ::
;;;;   single-escape-character | decimal-digit | x | u
;;;; hex-escape-sequence ::
;;;;   x hex-digit hex-digit
;;;; unicode-escape-sequence ::
;;;;   u hex-4-digits | u \{ hex-digits \}
;;;; hex-4-digits ::
;;;;   hex-digit hex-digit hex-digit hex-digit
;;;; regular-expression-literal ::
;;;;   / regular-expression-body / regular-expression-flags
;;;; regular-expression-body ::
;;;;   regular-expression-first-char regular-expression-chars
;;;; regular-expression-chars ::
;;;;   [] | regular-expression-chars regular-expression-char
;;;; regular-expression-first-char ::
;;;;   regular-expression-non-terminator \ [* | \\ | / | \[] |
;;;;   regular-expression-backslash-sequence | regular-expression-class
;;;; regular-expression-char ::
;;;;   regular-expression-non-terminator \ [\\ | / | \[] |
;;;;   regular-expression-backslash-sequence | regular-expression-class
;;;; regular-expression-backslash-sequence ::
;;;;   \\ regular-expression-non-terminator
;;;; regular-expression-non-terminator ::
;;;;   source-character \ line-terminator
;;;; regular-expression-class ::
;;;;   \[ regular-expression-class-chars \]
;;;; regular-expression-class-chars ::
;;;;   [] | regular-expression-class-chars regular-expression-class-char
;;;; regular-expression-class-char ::
;;;;   regular-expression-none-terminator \ [\] | \\]
;;;;   regular-expression-backslash-sequence
;;;; regular-expression-flags ::
;;;;   [] | regular-expression-flags identifier-part
;;;; template ::
;;;;   no-substitution-template | template-head
;;;; no-substitution-template ::
;;;;   ` (template-characters) `
;;;; template-head ::
;;;;   ` (template-characters) $ \{
;;;; template-substitution-tail ::
;;;;   template-middle | template-tail
;;;; template-middle ::
;;;;   \} (template-characters) $ \{
;;;; template-tail ::
;;;;   \} (template-characters) `
;;;; template-characters ::
;;;;   template-character (template-characters)
;;;; template-character ::
;;;;   $ | \\ escape-sequence | line-continuation | line-terminator-sequence |
;;;;   source-character \ [` | \\ | $ | line-terminator]
