# Useful notes of ECMA standard to implement

## Terms
**Type** includes *undefined*, *null*, *boolean*, *string*, *symbol*, *number*
and *object*. To implement **type**, define a global value that contains all of
them.  
**Primitive value** includes type *undefined*, *null*, *boolean*, *number*,
*string* and *symbol*. To implement **primitive value**, define a global value
that contains all of them.  
**Object** is a collection of properties, has a single prototype object, which
may be null. To implement **object**, define a prototype class which contains
all slots.  
**Constructor** is a function object that creates and initializes objects,
the value of a constructor's prototype property is a prototype object that is
used to implement inheritance and shared properties. To implement
**constructor**, define a generic function with the same name and define
concrete methods on the prototype class, which will initiate all inherited
slots by default. If a subclass method is called, firstly call the most
specific method, and CALL-NEXT-METHOD.  
**Prototype** is object that provides shared properties for other objects,
properties added to an object's prototype are shared through inheritance, by
all objects sharing the prototype. Or a new object may be created with an
explicitly specified prototype by using the `Object.create` function.
To implement **prototype**, for every class, no matter user defined or builtin,
make a prototype class and make a object class to be the subclass of it. e.g.
User defined a class `a` and `b extends a`, then we should get classes
`a` and `b`. Here `b` has prototype `a`.  
**Ordinary object** is object that has the default behaviour for the essential
internal methods that must be supported by all objects. It will only be a
definition rather than a concrete type.  
**Exotic object** is object that does not have the default behaviour for one
or more of the essential internal methods that must be supported by all
objects. It is the complement of **ordinary object**, so there won't be a
concrete type.  
**Undefined value** and **undefined type** are used when a variable has not
been assigned a value. To implement them, in the very-first pass of IR handling
we assign every uninitialized variable type **undefined**.  
**Null value** and **null type** are used when we want to represent the
intentional absense of any object value. So every object can be **null** or
of its type, to be simple, use NIL instead, when compiling, if we see NIL in
the ES land, we know it's **null**.  
**Boolean value** and **boolean type** are **true** and **false**, since in
Lisp we only have T and NIL, to implement **true** and **false**, we only have
to attach **boolean type** to the variable (or node in IR), and use 1 and 0.  
**Boolean object** is an instance of the standard **Boolean** constructor,
it can be coerced into a Boolean value, to implement Boolean object, like other
objects, we have a Boolean prototype class and a Boolean class, which is
trivial extension, and constructor the generic function to make a Boolean.  
**String value** and **string type** are finite ordered sequence of zero or
more 16-bit unsigned integer, so it looks like **UInt16Array** in some aspect.
To implement a string, keep in mind that it's a stringified UInt16Array.  
**String object** is implemented like **Boolean object** above.  
**Number value**, **Number type** and **Number object** are implemented like
those above.
**Infinity** will be reported if operand overflows, if it appears between
calculation, when it occurs, return **Infinity**, by comparing.  
**NaN** is Not-a-Number value, will be a symbol, and when met, in IR stage
or runtime stage, return **NaN** and the outer handles it.  
**Symbol value**, **symbol type** and **symbol object** are implemented like
those above.  
**Function** is different, now we don't support external executable, so as a
general ES subroutine. To implement a **function**, since functions won't be
extendable, so a function is a bare-bone class with properties.  
**Built-in function** is a built-in function object.  
**Property** is part of an object that associates a key (a string or symbol)
and a value. To implement **property**, accept every string or symbol key, and
find such slot, if present, do something, if not, add the key and do something.  
**Method** is a function that is the value of a property. To implement
**method**, consider declare such slot type `(or function nil)`.  
**Built-in method** is a method that is a built-in function.  
**Attribute** is a internal value that defines some characteristic of a
property. A property is a structure with different attributes.  
**Own property** is property directly contained by its object, and
**inherited property** is its complement.

## Well-known symbols
If a well-known symbol is a method, call this function after optimization
stage, if it is a boolean valued property or a string valued property,
embed it into corresponding prototypes, and implement them. If it is a function
valued property, make generic function and methods. If object valued, the type
will be `(or object null)`.

## The relation between \[\[Prototype\]\] and .prototype
If we have a user-defined class `a`, itself is a function, so its
`\[\[Prototype\]\]` internal slot's value is `Function.prototype`, which we
will use `-function` to implement. A internal class won't have this internal
attribute but store this as a property, so `Function` only have
`Function.prototype`, which point to the class itself in order to make
implementation easy. Now we have two classes, `b extends a`, then:  

    `b\[\[Prototype\]\] === a`
	`b.prototype === b.prototype`
	`a\[\[Prototype\]\] === Function === Function.prototype`
	`a.prototype === a.prototype`
	`b.prototype\[\[Prototype\]\] === a.prototype`
	`a.prototype\[\[Prototype\]\] === Object.prototype === Object`

Here `Function.prototype === Function` and `Object.prototype === Object` is
implementation specifical.
