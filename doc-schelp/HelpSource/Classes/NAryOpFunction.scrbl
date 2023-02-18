#lang scribble/manual
@(require (for-label racket))

@title{NAryOpFunction}
represent a n-ary operation on a function@section{categories}
 Core
@section{related}
 Classes/BinaryOpFunction, Classes/UnaryOpFunction, Classes/NAryOpStream, Classes/Pnaryop, Overviews/Operators

@section{description}

Operating on functions instead of numbers, what results is not a result of the calculation, but a structure that represents that calculation.

@section{examples}
 

@racketblock[
// example
a = 0.8.linexp(0, 1, 40, 20000); // map (0..1) to exponentially to human frequency hearing range
a = { b }.linexp(0, 1, 40, 20000); // result is  a NAryOpFunction
b = 0.1;
a.value; // now it is evaluated, and the result is calculated
b = 0.5;
a.value; // again, with a different value.
::

]

@racketblock[
// sound example
(
var a = { 1.0.rand };
var b = a.linexp(0, 1, 40, 20000);
fork {
	15.do {
		(instrument: \default, freq: b.value).play;
		0.3.wait;
	}
}
)
::
]


