#lang scribble/manual
@(require (for-label racket))

@title{BinaryOpFunction}
 represent a binary operation on a function@section{categories}
  Core
@section{related}
  Classes/UnaryOpFunction, Classes/NAryOpFunction, Classes/BinaryOpStream, Classes/Pbinop, Overviews/Operators

@section{description}


Operating on functions instead of numbers, what results is not a result of the calculation, but a structure that represents that calculation.

@section{instanceMethods}
 

@section{private}
 storeOn

@section{method}
 value

Executes each of the operand functions and then performs the selector on the result.

@section{method}
 valueArray

the same as link::#-value::

@section{examples}
 


@racketblock[
// example
a = 5 + 7; // result is 12.
a = { b } + 7; // result is  a BinaryOpFunction
b = 5;
a.value; // now it is evaluated, and the result is calculated
b = 8;
a.value; // again, with a different value.
::

]

@racketblock[
// sound example
(
var a = { 19.rand };
var b = { [5, 8, 9].choose };
var c = a + b;
fork {
	15.do {
		(instrument: \default, note: [c.value, a.value]).play;
		0.3.wait;
	}
}
)
::
]

