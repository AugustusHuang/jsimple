# From abstract operations to Lisp style forms

## Type conversion
Type conversion is trivial, use `typecase`. e.g.  

`ToObject(argument)` :  
`(typecase argument  
   (-undefined (error '-type-error)  
   (-null (error '-type-error)  
   (-boolean (make-instance '-boolean-prototype))`  
...  

`ToPropertyKey(argument)` :  
`(let ((key (to-primitive argument string)))  
   (typecase key  
     (-symbol key)  
	 (t (to-string key))))`  

`ToLength(argument)` :  
`(let ((length (to-integer argument))))`, if argument is negative, return 0,
if argument is infinity, return 2^53-1,
else return `(min argument (- (expt 2 53) 1))`.  

`CanonicalNumericIndexString(argument)` :  
`(declare (type string argument))`, if argument is "-0", return 0,
`(let ((num (to-number argument))))`, if `SameValue(ToString(num), argument)`
is false, return `:undefined`, or return `num`.

## Testing and comparison operations
`RequireObjectCoercible(argument)` acts like a type conversion, use `typecase`.  

`isXXX` functions checks a value's type and internal slots, check the prototype
list and judge, then check the internal slots.  

`SameValue(x, y)` :  
`(when (not (eql (-type x) (-type y))) :false)`  
`(when (-typep x '-undefined) :true)`  
`(when (-typep x '-null) :true)`  
And other types...  

## Operations on objects
These operations are implemented using generic functions. The internal methods
mentioned in these operations are also implemented using generic functions.

## Operations on iterator objects
`GetIterator(obj, method)` :  
If `method` is not passed, use internal iterator method, or call `method` with
checking its type is Object.  

# Executable code and execution contexts
Lexical environment is associated with some specific syntactic structure of
code such as function declaration, block statement, catch clause of a try
statement, so a new lexical environment is created each time such code is
evaluated, in the parser form, they are :DEFUN, :BLOCK, :CATCH in :TRY.
