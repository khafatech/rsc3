#lang scribble/manual
@(require (for-label racket))

@title{Number}
 Mathematical quantity@section{categories}
  Math

@section{description}

Number represents a mathematical quantity.

@section{instancemethods}
 

@section{subsection}
  Math

@section{method}
  +
Addition.

@section{method}
  -
Subtraction.

@section{method}
  *
Multiplication.

@section{method}
  /
Division.

@section{method}
  div
Integer division.

@section{method}
  %
Modulo.

@section{method}
  **
Exponentiation.


@section{subsection}
  Polar Coordinate Support

@section{method}
  rho
Answer the polar radius of the number.

@section{method}
  theta
Answer the polar angle of the number.


@section{subsection}
  Complex Number Support

@section{method}
  real
Answer the real part of the number.

@section{method}
  imag
Answer the imaginary part of the number.


@section{subsection}
  Conversion

@section{method}
  @
Create a new link::Classes/Point:: whose x coordinate is the receiver and whose y coordinate is aNumber.

@section{method}
  complex
Create a new link::Classes/Complex:: number whose real part is the receiver with the given imaginary part.

@section{method}
  polar
Create a new link::Classes/Polar:: number whose radius is the receiver with the given angle.


@section{subsection}
  Iteration

@section{method}
  for
Calls strong::function:: for numbers from this up to endval, inclusive, stepping each time by 1.
@section{argument}
  endValue
a link::Classes/Number::.
@section{argument}
  function
a link::Classes/Function:: which is passed two arguments, the first which is an number from this to
@section{argument}
  endval, and the second which is a number from zero to the number of iterations minus one.

@section{method}
  forBy
Calls strong::function:: for numbers from this up to endval stepping each time by step.
@section{argument}
  endValue
a link::Classes/Number::.
@section{argument}
  stepValue
a link::Classes/Number::.
@section{argument}
  function
a link::Classes/Function:: which is passed two arguments, the first which is an number from this to
endval, and the second which is a number from zero to the number of iterations minus one.

@section{method}
  forSeries
Calls strong::function:: for numbers from this up to endval stepping each time by a step specified by second.
@section{argument}
  second
a link::Classes/Number::.
@section{argument}
  last
a link::Classes/Number::.
@section{argument}
  function
a link::Classes/Function:: which is passed two arguments, the first which is an number from this to
endval, and the second which is a number from zero to the number of iterations minus one.



