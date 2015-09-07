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
all slots, those global inherited slots will be allocated by class.  
**Constructor** is a function object that creates and initializes objects,
the value of a constructor's prototype property is a prototype object that is
used to implement inheritance and shared properties. To implement
**constructor**, define a data class object with the same name and decide
to include `[[Call]]` and `[[Construct]]` or not, it's only a pure
data class, only the internal methods will be callable.  
**Prototype** is object that provides shared properties for other objects,
properties added to an object's prototype are shared through inheritance, by
all objects sharing the prototype. Or a new object may be created with an
explicitly specified prototype by using the `Object.create` function.
To implement **prototype**, for every class, no matter user defined or builtin,
make a prototype class and make a object class to be the subclass of it. e.g.
User defined a class `a` and `b extends a`, then we should get classes
`a` and `b`. Here `b` has prototype `a`.  
The relation between **constructor** and **prototype**:  
Generally, `new XXX()` will be compiled into a construct call, which will
be an class-wise internal slot in **constructor** class object. `XXX()` will
be compiled into a call call, if those calls are `:NULL`, throw an error.
There will be a `constructor` property in every **prototype** and there
will be a `prototype` property in every **constructor**, if not specified.
`ObjectPrototype` is the very beginning, it has no ancestor.  
Difference between `[[Prototype]]` and `prototype`:  
`[[Prototype]]` is implemented as `-prototype` in Lisp, and it contains
the symbol of the direct ancestor of a class, e.g. `ObjectPrototype` has no
ancestor so `[[Prototype]]` is `:NULL`, `Object`'s `[[Prototype]]`
is `FunctionPrototype` but its `prototype` is `ObjectPrototype`.  
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
of its type, to be simple, use `:NULL`, when compiling, if we see `:NULL`,
we know it's **null**.  
**Boolean value** and **boolean type** are **true** and **false**, since in
Lisp we only have T and NIL, to implement **true** and **false**, we only have
to attach **boolean type** to the variable (or node in IR),
and use `:TRUE` and `:FALSE`.  
**Boolean object** is an instance of the standard **Boolean** constructor,
it can be coerced into a Boolean value, to implement Boolean object, like other
objects, we have a Boolean prototype class and a Boolean class, which is
trivial extension.  
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
general ES subroutine. To implement a **function**,
use the internal methods or properties in `Function` and `FunctionPrototype`.  
**Built-in function** is a built-in function object.
Have to define them somewhere, since they have their own properties.  
**Property** is part of an object that associates a key (a string or symbol)
and a value. To implement **property**, accept every string or symbol key, and
find such slot, if present, do something, if not, add the key and do something.  
**Method** is a function that is the value of a property. To implement
**method**, consider declare such slot type `(or symbol nil)`
and use `SYMBOL-FUNCTION`, or use generic functions.  
**Built-in method** is a method that is a built-in function.  
**Attribute** is a internal value that defines some characteristic of a
property. A property is a structure with different attributes.  
**Own property** is property directly contained by its object, and
**inherited property** is its complement, here we don't tell this different,
but some of the properties are class-wise, if a property is attached to a
class, a change of it will leads to change of all, then we move it into
the `properties` slot, or we take it out and make a instance allocation.

## Well-known symbols
If a well-known symbol is a method, call this function after optimization
stage, if it is a boolean valued property or a string valued property,
embed it into corresponding prototypes, and implement them. If it is a function
valued property, make generic function and methods. If object valued, the type
will be `(or object null)`.

## The relation between `[[Prototype]]` and `.prototype`
If we have a user-defined class `a`, itself is a function, so its
`[[Prototype]]` internal slot's value is `Function.prototype`, which we
will use `-function-prototype` to implement.
A internal class won't have this internal attribute but store this as a property,
so `Function` only have `Function.prototype`,
which point to the prototype class in order to make implementation easy.
Now we have two classes, `b extends a`, then:  

    `b[[Prototype]] === a`
    `b.prototype === b.prototype === b-prototype`
    `a[[Prototype]] === Function === -function`
    `a.prototype === a.prototype === a-prototype`
    `b.prototype[[Prototype]] === a.prototype`
    `a.prototype[[Prototype]] === Object.prototype === -object-prototype`


