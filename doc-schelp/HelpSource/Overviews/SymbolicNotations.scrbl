#lang scribble/manual
@(require (for-label racket))

@title{Symbolic Notations}
 Catalog of symbolic notations in SuperCollider@section{categories}
  Language
@section{related}
  Overviews/Operators, Reference/Syntax-Shortcuts

@section{section}
  Arithmetic operators

Math operators apply to many classes, including arrays and other collections.

Using a basic math operator on a Symbol swallows the operation (returns the symbol)

@racketblock[
\symbol * 5
symbol
::

]
@section{definitionlist}
 
## 
@racketblock[ number + number :: || addition
## ]

@racketblock[ number - number :: || subtraction
## ]

@racketblock[ number * number :: || multiplication
## ]

@racketblock[ number / number :: || division
## ]

@racketblock[ number % number :: || modulo
## ]

@racketblock[ number ** number :: || exponentiation
::

]
@section{section}
  Bitwise arithmetic
@section{definitionlist}
 
## 
@racketblock[ number & number :: || bitwise and
## ]

@racketblock[ number | number :: || bitwise or
## ]

@racketblock[ number << number :: || bitwise left shift
## ]

@racketblock[ number >> number :: || bitwise right shift
## ]

@racketblock[ number +>> number :: || unsigned bitwise right shift
::

]
@section{section}
  Logical operators
@section{definitionlist}
 
## 
@racketblock[ object == object :: || equivalence
## ]

@racketblock[ object === object :: || identity
## ]

@racketblock[ object != object :: || not equal to
## ]

@racketblock[ object !== object :: || not identical to
::

Objects may be equivalent but not identical.
]

@racketblock[
[1, 2, 3] == [1, 2, 3]
true
[1, 2, 3] === [1, 2, 3]
false       // a and b are two different array instances with the same contents

a = b = [1, 2, 3];
a === b;
true        // a and b are the same array instance
::

]
@section{definitionlist}
 
## 
@racketblock[ number < number :: || comparison (less than)
## ]

@racketblock[ number <= number :: || comparison (less than or equal to)
## ]

@racketblock[ number > number :: || comparison (greater than)
## ]

@racketblock[ number >= number :: || comparison (greater than or equal to)
::
]
@section{definitionlist}
 
## 
@racketblock[ boolean && boolean :: || logical And
## ]

@racketblock[ boolean || boolean :: || logical Or
::
When a function is the second operand, these operators perform short-circuiting (i.e., the function is executed only when its result would influence the result of the operation). This is recommended for speed.

With ]

@racketblock[ and: :: and ]

@racketblock[ or: :: second-argument functions will be inlined. If you use ]

@racketblock[&&:: or ]

@racketblock[||::, no inlining will be done and performance will be slower.
]

@racketblock[
a = 1;

a == 1 and: { "second condition".postln; [true, false].choose }
second condition
true

a == 1 or: { "second condition".postln; [true, false].choose }
true

a != 1 and: { "second condition".postln; [true, false].choose }
false

a != 1 or: { "second condition".postln; [true, false].choose }
second condition
true
::
In this case, the second condition will cause an error if a is nil, because nil does not understand addition. a.notNil is a safeguard to ensure the second condition makes sense.
]

@racketblock[
a = nil;
a.notNil and: { "second condition".postln; (a = a+1) < 5 }
false

a = 10;
a.notNil and: { "second condition".postln; (a = a+1) < 5 }
second condition
false
::

]
@section{section}
  Array and Collection operators

@section{definitionlist}
 
## 
@racketblock[ object ++ object :: || concatenation
## ]

@racketblock[ collection +++ collection :: || lamination (see link::Guides/J-concepts-in-SC::)
## ]

@racketblock[ collection @ index :: || collection/array indexing: .at(index) or [index]
## ]

@racketblock[ collection @@ integer :: || collection/array indexing: .wrapAt(int)
## ]

@racketblock[ collection @|@ integer :: || collection/array indexing: .foldAt(int)
## ]

@racketblock[ collection |@| integer :: || collection/array indexing: .clipAt(int)
::

]
@section{section}
  Set operators
@section{definitionlist}
 
## 
@racketblock[ set & set :: || intersection of two sets
## ]

@racketblock[ set | set :: || union of two sets
## ]

@racketblock[ setA - setB :: || difference of sets (elements of setA not found in setB)
## ]

@racketblock[ set -- set :: || symmetric difference:
]

@racketblock[
(setA -- setB) == ((setA - setB) | (setB - setA))
::
::

]

@racketblock[
a = Set[2, 3, 4, 5, 6, 7];
b = Set[5, 6, 7, 8, 9];

a - b
Set[ 2, 4, 3 ]

b - a
Set[ 8, 9 ]

((a-b) | (b-a))
Set[ 2, 9, 3, 4, 8 ]

a -- b
Set[ 2, 9, 3, 4, 8 ]
::

]
@section{section}
  Geometry operators
@section{definitionlist}
 
## 
@racketblock[ number @ number :: || make a link::Classes/Point:: of two numbers
]

@racketblock[
x @ y
// returns:
Point(x, y)
::
## ]

@racketblock[ point @ point :: || make a link::Classes/Rect:: of two link::Classes/Point::s
]

@racketblock[
Point(left, top) @ Point(right, bottom)
// returns:
Rect(left, top, right-left, bottom-top)
::
## ]

@racketblock[ ugen @ ugen :: || create a Point with two link::Classes/UGen::s
## ]

@racketblock[ rect & rect :: || intersection of two rectangles
## ]

@racketblock[ rect | rect :: || union of two rectangles (returns a Rect whose boundaries exactly encompass both Rects)
::

]
@section{section}
  IOStream operators
@section{definitionlist}
 
## 
@racketblock[ stream << object :: || represent the object as a string and add to the stream.
A common usage is with the Post class, to write output to the post window.
]

@racketblock[
Post << "Here is a random number: " << 20.rand << ".\n";
Here is a random number: 13.
::

## ]

@racketblock[ stream <<* collection :: || add each item of the collection to the stream.
]

@racketblock[
Post << [0, 1, 2, 3]
[ 0, 1, 2, 3 ]

Post <<* [0, 1, 2, 3]
0, 1, 2, 3
::

## ]

@racketblock[ stream <<< object :: || add the object's compile string to the stream.
]

@racketblock[
Post <<< "a string"
"a string"
::
## ]

@racketblock[ stream <<<* collection :: || add each item's compile string to the stream.
::

]
@section{section}
  Conditional execution operators
@section{definitionlist}
 
## 
@racketblock[ object ? object :: || nil check (no .value)
## ]

@racketblock[ object ?? function :: || nil check (.value, function is inlined)
If the object is nil, the second expression's value will be used; otherwise, it will be the first object.
]

@racketblock[
a = [nil, 5];

10.do({ (a.choose ? 20.rand).postln });
10.do({ (a.choose ?? { 20.rand }).postln });
::
]

@racketblock[ ?? { } :: is generally recommended. ]

@racketblock[?:: always evaluates the second expression, even if its value will not be used.
]

@racketblock[ ?? :: evaluates the function conditionally (only when needed).
If the function defines no variables, the function will be inlined for speed.

Especially useful when the absence of an object requires a new object to be created. In this example, it's critical that a new Slider not be created if the object was already passed in.
]

@racketblock[
f = { |slider, parent|
    slider = slider ?? { Slider.new(parent, Rect(0, 0, 100, 20)) };
    slider.value_(0);
};
::
If the first line inside the function instead read ]

@racketblock[
slider = slider ? Slider.new(parent, Rect(0, 0, 100, 20));
::
, a new slider would be created even if it is not needed, or used.

## ]

@racketblock[ object !? function :: || execute function if object is not nil.
]

@racketblock[
a = [10, nil].choose;
a !? { "ran func".postln };
// equivalent of:
if (a.notNil) { "ran func".postln };
::
Used when an operation requires a variable not to be empty.
]

@racketblock[
f = { |a| a + 5 };
f.value
// error: nil does not understand +

f = { |a| a !? { a+5 } };
f.value
nil // no error
f.value(2)
7
::
::

]
@section{section}
  Miscellaneous operators
@section{definitionlist}
 
## 
@racketblock[ object ! number :: || same as ]

@racketblock[ object.dup(number) ::
]

@racketblock[
15 ! 5
[ 15, 15, 15, 15, 15 ]
::
If the object is a function, it behaves like Array.fill(number, function).
]

@racketblock[
{ 10.rand } ! 5
[ 8, 9, 3, 8, 0 ]
::
## ]

@racketblock[ object -> object :: || creates an link::Classes/Association::, used in dictionaries.
## ]

@racketblock[ expression <! expression :: || bypass value of second expression.
This operator evaluates both expressions, and returns the value of the first.
]

@racketblock[
a = 0;
0

// a is incremented twice, but the return value (1)
// comes from the first increment (0 + 1)
(a = a + 1) <! (a = a + 1)
1

a	// a's value reflects both increments
2
::

## ]

@racketblock[ function <> function :: || function composition operator.
This operator returns a new function, which evaluates the second function and passes the result to the first function.
]

@racketblock[
f = { |a| a * 5 } <> {|a| a + 2 };
f.(10);
60                  // == (10+2) * 5
::
An array as argument is passed through the chain:
]

@racketblock[
f.([10, 75, 512]);
[ 60, 385, 2570 ]   // == ([10, 75, 512]+2) * 5
::
::

]
@section{section}
  Symbolic notations to define literals/other objects
@section{definitionlist}
 
## 
@racketblock[ $ :: || character prefix: ]

@racketblock[ "ABC".at(0) == $A ::
## ]

@racketblock[ '' :: or ]

@racketblock[ \ :: || define a literal link::Classes/Symbol:: : ]

@racketblock[ 'abc' === \abc ::
## ]

@racketblock[ "" :: || define a literal link::Classes/String:: : ]

@racketblock[ "SuperCollider is the best" ::
## ]

@racketblock[ [item, item...] :: || define an link::Classes/Array:: containing given items
## ]

@racketblock[ Set[item, item...] :: || define a link::Classes/Set:: -- any link::Classes/Collection:: class name can be used other than Set
## ]

@racketblock[ #[item, item...] :: || define a literal link::Classes/Array::
## ]

@racketblock[ (a:1, b:2) :: || define an link::Classes/Event:: (same as ]

@racketblock[ Event[\a -> 1, \b -> 2] ::)
## ]

@racketblock[ ` :: (backtick or backquote) || define a link::Classes/Ref:: : ]

@racketblock[ `1 == Ref(1), `(a+1) == Ref(a+1) ::
## ]

@racketblock[ \ :: || inside a string or symbol, escapes the next character
]

@racketblock[
"abc\"def\"ghi"
abc"def"ghi

'abc\'def\'ghi'
abc'def'ghi
::
]
@section{definitionlist}
 
## 
@racketblock[ \t :: || tab character
## ]

@racketblock[ \n :: || newline character
## ]

@racketblock[ \l :: || linefeed character
## ]

@racketblock[ \r :: || carriage return character
## ]

@racketblock[ \\ :: || \ character
::

## ]

@racketblock[ { } :: || define an open function
## ]

@racketblock[ #{ } :: || define a closed function
## ]

@racketblock[ (_ * 2) :: || define a function ]

@racketblock[ { |a| a * 2 } :: (see link::Reference/Partial-Application::)
::

]
@section{section}
  Argument definition
@section{definitionlist}
 
## 
@racketblock[ |a, b, c| :: || define function/method arguments
## ]

@racketblock[ |a, b ... c| :: || define function/method arguments; arguments after a and b will be placed into c as an array
## ]

@racketblock[ #a, b, c = myArray ::|| assign consecutive elements of myArray to multiple variables
## ]

@racketblock[ #a, b ... c = myArray :: || assign first two elements to a and b; the rest as an array into c
::

]
@section{section}
  Where f is a function
@section{definitionlist}
 
## 
@racketblock[ f.( ) :: || evaluate the function with the arguments in parentheses
## ]

@racketblock[ f.(*argList) :: || evaluate the function with the arguments in an array
## ]

@racketblock[ f.(anArgName: value) :: || keyword addressing of function or method arguments
]

@racketblock[
f = { |a, b| a * b };
f.(2, 4);
f.(*[2, 4]);
f.(a: 2, b: 4);
::
## ]

@racketblock[ SomeClass.[index] :: || Equivalent to SomeClass.at(index) -- Instr.at is a good example
## ]

@racketblock[ myObject.method(*array) :: || call the method with the arguments in an array
## ]

@racketblock[ obj1 method: obj2 :: || same as ]

@racketblock[obj1.method(obj2):: or ]

@racketblock[method(obj1, obj2)::.
This works only with single-argument methods like binary operators.
::

]
@section{section}
  Class and instance variable access

Inside a class definition (see link::Guides/WritingClasses:: ):

@racketblock[
{
    classvar <a,    // Define a class variable with a getter method (for outside access)
             >b,    // Define a class variable with a setter method
             <>c;   // Define a class variable with both a getter and setter method

    var      <a,    // Define an instance variable with a getter method (for outside access)
             >b,    // Define an instance variable with a setter method
             <>c;   // Define an instance variable with both a getter and setter method

    // methods go here ...
}
::
These notations do not apply to variables defined within methods.

]
@section{definitionlist}
 
## 
@racketblock[ ^someExpression :: || Inside a method definition: return the expression's value to the caller
## ]

@racketblock[ instVar_ { } :: || define a setter for an instance variable
## ]

@racketblock[ myObject.instVar = x; :: || invoke the setter: ]

@racketblock[ (myObject.instVar_(x); x) ::
::

]
@section{section}
  Array series and indexing
@section{definitionlist}
 
## 
@racketblock[ (a..b) :: || produces an array consisting of consecutive integers from a to b
## ]

@racketblock[ (a, b..c) :: || e.g.: (1, 3..9) produces [1, 3, 5, 7, 9]
## ]

@racketblock[ (..b) :: || produces an array 0 through b
## ]

@racketblock[ (a..) :: || not legal (no endpoint given)

## ]

@racketblock[ a[i..j] :: || same as ]

@racketblock[ a.copySeries(i, j) :: (see link::Classes/ArrayedCollection#-copySeries::)
## ]

@racketblock[ a[i, j..k] :: || e.g.: ]

@racketblock[ a[1, 3..9] :: retrieves array elements 1, 3, 5, 7, 9
## ]

@racketblock[ a[..j] :: || same as ]

@racketblock[ a.copySeries(0, j) ::
## ]

@racketblock[ a[j..] :: || same as ]

@racketblock[ a.copySeries(i, a.size-1) :: (this is OK--Array is finite)

## ]

@racketblock[ ~ :: || access an environment variable
## ]

@racketblock[ ~abc :: || compiles to ]

@racketblock[ \abc.envirGet ::
## ]

@racketblock[ ~abc = value :: || compiles to ]

@racketblock[ \abc.envirPut(value) ::
::

]
@section{section}
  Adverbs to math operators
(see link::Reference/Adverbs:: )

e.g.:

@racketblock[
[1, 2, 3] * [2, 3, 4]
[ 2, 6, 12 ]

[1, 2, 3] *.t [2, 3, 4]
[ [ 2, 3, 4 ], [ 4, 6, 8 ], [ 6, 9, 12 ] ]
::
]
@section{definitionlist}
 
## 
@racketblock[ .s :: || output length is the shorter of the two arrays
## ]

@racketblock[ .f :: || use folded indexing instead of wrapped indexing
## ]

@racketblock[ .t :: || table-style
## ]

@racketblock[ .x :: || cross (like table, except that the results of each operation are concatenated, not added as another dimension)
## ]

@racketblock[ .0 :: || operator depth (see link::Guides/J-concepts-in-SC:: )
## ]

@racketblock[ .1 :: || etc.
::

]


