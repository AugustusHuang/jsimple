/* Some equivalent codes from es6-features.org by Ralf S.Engelschall.
   Ensure that these two version will both be accepted by the parser.
*/

// ES6 const feature.
const PI = 3.141593;
// PI > 3.0;

// ES6 block scoped variables.
for (let i = 0; i < a.length; i++) {
    let x = a[i];
    // Something more.
}
for (let i = 0; i < b.length; i++) {
    let y = b[i];
    // Something more.
}

let callbacks = [];
for (let i = 0; i <= 2; i++) {
    callbacks[i] = function () { return i * 2; };
}
// callbacks[0]() === 0;
// callbacks[1]() === 2;
// callbacks[2]() === 4;

// ES6 with block scoped functions.
{
    function foo () { return 1; }
    // foo() === 1;
    {
	function foo () { return 2; }
	// foo() === 2;
    }
    // foo() === 1;
}

// ES6 with expression bodies.
odds  = evens.map(v => v + 1);
pairs = evens.map(v => ({ even: v, odd: v + 1 }));
nums  = evens.map((v, i) => v + i);

// ES6 with statement bodies.
nums.forEach(v => {
    if (v % 5 === 0)
	fives.push(v);
})

// ES6 with lexical this.
this.nums.forEach((v) => {
    if (v % 5 === 0)
	this.fives.push(v);
});

// ES6 default parameter values.
function f (x, y = 7, z = 42) {
    return x + y + z;
}
// f(1) === 50;

// ES6 rest parameter.
function f (x, y, ...a) {
    return (x + y) * a.length;
}
// f(1, 2, "hello", true, 7) === 9;

// ES6 spread operator.
var params = [ "hello", true, 7 ];
var other = [ 1, 2, ...params ]; // [ 1, 2, "hello", true, 7 ]
// f(1, 2, ...params) === 9;

var str = "foo";
var chars = [ ...str ]; // [ "f", "o", "o" ]

// ES6 string interpolation.
var customer = { name: "Foo" };
var card = { amount: 7, product: "Bar", unitprice: 42 };
message = `Hello ${customer.name},
want to buy ${card.amount} ${card.product} for
    a total of ${card.amount * card.unitprice} bucks?`;

// ES6 custom interpolation.
get`http://example.com/foo?bar=${bar + baz}&quux=${quux}`;

// ES6 raw string access.
function quux (strings, ...values) {
    strings[0] === "foo\n";
    strings[1] === "bar";
    strings.raw[0] === "foo\\n";
    strings.raw[1] === "bar";
    values[0] === 42;
}
quux `foo\n${ 42 }bar`

// String.raw `foo\n${ 42 }bar` === "foo\\n42bar";

// ES6 binary & octal literal.
0b111110111 === 503;
0o767 === 503;

// ES6 property shorthand.
obj = { x, y };

// ES6 computed property names.
obj = {
    foo: "bar",
    [ "prop_" + foo() ]: 42
};

// ES6 method properties.
obj = {
    foo (a, b) {
	// Something.
    },
    bar (x, y) {
	// Something.
    },
        *quux (x, y) {
	    // Something.
	}
};

// ES6 array matching.
var list = [ 1, 2, 3 ];
var [ a, , b ] = list;
[ b, a ] = [ a, b ];

// ES6 object matching, shorthand notation.
var { op, lhs, rhs } = getASTNode();

// ES6 object matching, deep matching.
var { op: a, lhs: { op: b }, rhs: c } = getASTNode();

// ES6 parameter context matching.
function f ([ name, val ]) {
    console.log(name, val);
}
function g ({ name: n, val: v }) {
    console.log(n, v);
}
function h ({ name, val }) {
    console.log(name, val);
}
f([ "bar", 42 ]);
g({ name: "foo", val:  7 });
h({ name: "bar", val: 42 });

// ES6 fail-soft destructuring.
var list = [ 7, 42 ];
var [ a = 1, b = 2, c = 3, d ] = list;
// a === 7;
// b === 42;
// c === 3;
// d === undefined;

// ES6 symbol export/import.
//  lib/math.js
export function sum (x, y) { return x + y };
export var pi = 3.141593;

//  someApp.js
import * as math from "lib/math";
console.log("2π = " + math.sum(math.pi, math.pi));

//  otherApp.js
import { sum, pi } from "lib/math";
console.log("2π = " + sum(pi, pi));

// ES6 default & wildcard.
//  lib/mathplusplus.js
export * from "lib/math";
export var e = 2.71828182846;
export default (x) => Math.exp(x);

//  someApp.js
import exp, { pi, e } from "lib/mathplusplus";
console.log("e^{π} = " + exp(pi));

// ES6 class definition.
class Shape {
    constructor (id, x, y) {
	this.id = id;
	this.move(x, y);
    }
    move (x, y) {
	this.x = x;
	this.y = y;
    }
}

// ES6 class inheritance.
class Rectangle extends Shape {
    constructor (id, x, y, width, height) {
	super(id, x, y);
	this.width  = width;
	this.height = height;
    }
}
class Circle extends Shape {
    constructor (id, x, y, radius) {
	super(id, x, y);
	this.radius = radius;
    }
}

// ES6 inheritance from expressions.
var aggregation = (baseClass, ...mixins) => {
    let base = class _Combined extends baseClass {
	constructor (...args) {
	    super(...args);
	    mixins.forEach((mixin) => {
		mixin.prototype.initializer.call(this);
	    });
	}
    };
    let copyProps = (target, source) => {
	Object.getOwnPropertyNames(source)
	    .concat(Object.getOwnPropertySymbols(source))
	    .forEach((prop) => {
		if (prop.match(/^(?:constructor|prototype|arguments|caller|name|bind|call|apply|toString|length)$/))
		    return
		Object.defineProperty(target, prop, Object.getOwnPropertyDescriptor(source, prop))
	    })
    }
    mixins.forEach((mixin) => {
	copyProps(base.prototype, mixin.prototype);
	copyProps(base, mixin);
    });
    return base;
};

class Colored {
    initializer ()     { this._color = "white"; }
    get color ()       { return this._color; }
    set color (v)      { this._color = v; }
}

class ZCoord {
    initializer ()     { this._z = 0; }
    get z ()           { return this._z; }
    set z (v)          { this._z = v; }
}

class Shape {
    constructor (x, y) { this._x = x; this._y = y; }
    get x ()           { return this._x; }
    set x (v)          { this._x = v; }
    get y ()           { return this._y; }
    set y (v)          { this._y = v; }
}

class Rectangle extends aggregation(Shape, Colored, ZCoord) {}

var rect = new Rectangle(7, 42);
rect.z     = 1000;
rect.color = "red";
console.log(rect.x, rect.y, rect.z, rect.color);

// ES6 base class access.
class Shape {
    …
    toString () {
	return `Shape(${this.id})`
    }
}
class Rectangle extends Shape {
    constructor (id, x, y, width, height) {
	super(id, x, y);
	…
    }
    toString () {
	return "Rectangle > " + super.toString();
    }
}
class Circle extends Shape {
    constructor (id, x, y, radius) {
	super(id, x, y);
	…
    }
    toString () {
	return "Circle > " + super.toString();
    }
}

// ES6 static members.
class Rectangle extends Shape {
    …
    static defaultRectangle () {
	return new Rectangle("default", 0, 0, 100, 100);
    }
}
class Circle extends Shape {
    …
    static defaultCircle () {
	return new Circle("default", 0, 0, 100);
    }
}
var defRectangle = Rectangle.defaultRectangle();
var defCircle    = Circle.defaultCircle();

// ES6 getter and setter.
class Rectangle {
    constructor (width, height) {
	this.width  = width;
	this.height = height;
    }
    set width  (width)  { this._width = width;             }
    get width  ()       { return this._width;              }
    set height (height) { this._height = height;           }
    get height ()       { return this._height;             }
    get area   ()       { return this.width * this.height; }
};
var r = new Rectangle(50, 20);
// r.area === 1000;

// ES6 symbol type.
Symbol("foo") !== Symbol("foo");
const foo = Symbol();
const bar = Symbol();
typeof foo === "symbol";
typeof bar === "symbol";
let obj = {};
obj[foo] = "foo";
obj[bar] = "bar";
JSON.stringify(obj); // {}
Object.keys(obj); // []
Object.getOwnPropertyNames(obj); // []
Object.getOwnPropertySymbols(obj); // [ foo, bar ]

// ES6 global symbols.
Symbol.for("app.foo") === Symbol.for("app.foo")
    const foo = Symbol.for("app.foo");
const bar = Symbol.for("app.bar");
Symbol.keyFor(foo) === "app.foo";
Symbol.keyFor(bar) === "app.bar";
typeof foo === "symbol";
typeof bar === "symbol";
let obj = {};
obj[foo] = "foo";
obj[bar] = "bar";
JSON.stringify(obj); // {}
Object.keys(obj); // []
Object.getOwnPropertyNames(obj); // []
Object.getOwnPropertySymbols(obj); // [ foo, bar ]

// ES6 iterator & for-of operator.
let fibonacci = {
    [Symbol.iterator]() {
	let pre = 0, cur = 1;
	return {
	    next () {
		[ pre, cur ] = [ cur, pre + cur ];
		return { done: false, value: cur };
	    }
	};
    }
}

for (let n of fibonacci) {
    if (n > 1000)
	break;
    console.log(n);
}

// ES6 generator function and iterator protocol.
let fibonacci = {
        *[Symbol.iterator]() {
	    let pre = 0, cur = 1;
	    for (;;) {
		[ pre, cur ] = [ cur, pre + cur ];
		yield cur;
	    }
	}
}

for (let n of fibonacci) {
    if (n > 1000)
	break;
    console.log(n);
}

// ES6 generator function direct use.
function* range (start, end, step) {
    while (start < end) {
	yield start;
	start += step;
    }
}

for (let i of range(0, 10, 2)) {
    console.log(i); // 0, 2, 4, 6, 8
}

// ES6 generator matching.
let fibonacci = function* (numbers) {
    let pre = 0, cur = 1;
    while (numbers-- > 0) {
	[ pre, cur ] = [ cur, pre + cur ];
	yield cur;
    }
};

for (let n of fibonacci(1000))
    console.log(n);

let numbers = [ ...fibonacci(1000) ];

let [ n1, n2, n3, ...others ] = fibonacci(1000);

// ES6 generator control-flow.
function async (proc, ...params) {
    var iterator = proc(...params);
    return new Promise((resolve, reject) => {
	let loop = (value) => {
	    let result;
	    try {
		result = iterator.next(value);
	    }
	    catch (err) {
		reject(err);
	    }
	    if (result.done)
		resolve(result.value);
	    else if (   typeof result.value      === "object"
			&& typeof result.value.then === "function")
		result.value.then((value) => {
		    loop(value);
		}, (err) => {
		    reject(err);
		});
	    else
		loop(result.value);
	}
	loop();
    });
}

function makeAsync (text, after) {
    return new Promise((resolve, reject) => {
	setTimeout(() => resolve(text), after);
    });
}

async(function* (greeting) {
    let foo = yield makeAsync("foo", 300)
    let bar = yield makeAsync("bar", 200)
    let baz = yield makeAsync("baz", 100)
    return `${greeting} ${foo} ${bar} ${baz}`
}, "Hello").then((msg) => {
    console.log("RESULT:", msg); // "Hello foo bar baz"
})

// ES6 proxying.
let target = {
    foo: "Welcome, foo"
};
let proxy = new Proxy(target, {
    get (receiver, name) {
	return name in receiver ? receiver[name] : `Hello, ${name}`;
    }
});
// proxy.foo   === "Welcome, foo";
// proxy.world === "Hello, world";

// ES6 reflection.
let obj = { a: 1 };
Object.defineProperty(obj, "b", { value: 2 });
obj[Symbol("c")] = 3;
Reflect.ownKeys(obj); // [ "a", "b", Symbol(c) ]

/* NOTE: Some of the new features only add new built-in functions, which will
   not be tested at parser phase, we should omit them since it won't 
   introduce anything new in parser.
*/
