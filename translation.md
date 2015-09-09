# From abstract operations to Lisp style forms

## Executable code and execution contexts
Lexical environment is associated with some specific syntactic structure of
code such as function declaration, block statement, catch clause of a try
statement, so a new lexical environment is created each time such code is
evaluated, in the parser form, they are :DEFUN, :BLOCK, :CATCH in :TRY.
Do we need this in the LESP-BUILTIN package? If we can handle all of them
in IR stage? I think so.

## Notes
Runtime semantics may be implemented as a function, if there is a chance to
use Lisp internal or SBCL library function, use it, if the library function
is everywhere, include the package in the use list of current package.
Conditional compilation is not a problem at this very early stage.
Sometimes a Lisp internal function accepts more general form than its es
counterpart, use it, if there are alternatives, use the most specific one.

## Abstraction levels
There are three levels, es level, inter level and lisp level, lisp level is
only present to directly use Lisp functions, when we need to compare the two
values, like comparing 1 and 2, what we have are numbers in es level, the
comparison function is in inter level, and the direct handle is in lisp level,
the result will be wrapped into a boolean in es level.  
es level:    1 < 2 (of type number-type)  
inter level: (less 1 2) (of type NUMBER) (the function name is not exact)  
lisp level:  (< 1 2)  

lisp level return:  t (of type T)  
inter level return: (boolean t) (the function name is not exact)  
es level:           true (of type boolean-type)  

So every abstract operation will assume its arguments, return values in
es types, but internal, for the sake of speed and sound, use the best case.

## About function names
Internal function objects are wrapped by a funcallable instance, e.g.
if a function is called `assign`, then its funcallable instance will be
`!assign`, if some lisp function has the same name as ours, use `assign-es`,
and since there won't be lisp internal functions start with `!` (I hope so,
and it seems that it's right), so the funcallable instance will still be
`!assign`. Constructors are the same (but now I haven't changed them).
To handle such kinds of conversion, use CAMEL-TO-HYPHEN and HYPHEN-TO-CAMEL
in builtin-util.lisp, it is still not complete yet.  
Some of the keywords that should be specially handled: URI, NaN, etc.. They
will be added into those two functions mentioned above soon.
