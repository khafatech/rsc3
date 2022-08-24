#lang scribble/manual
@(require (for-label racket))

@title{Polar}
 Polar coordinates@section{related}
  Classes/Point, Classes/Complex
@section{categories}
  Math

@section{description}

Represents polar coordinates.

@section{classmethods}
 

@section{method}
  new
Create a new polar coordinate with the given radius, rho, and angle in radians, theta.



@section{instancemethods}
 

@section{subsection}
  Math
@section{method}
  +, -, *, /
The math operations of addition, subtraction, multiplication and division are accomplished by
first converting to complex numbers.

@section{method}
  scale
Scale the radius by some value.

@section{method}
  rotate
Rotate the angle by some value.

@section{method}
  neg
Rotate by pi.


@section{subsection}
  Conversion

@section{method}
  magnitude
Answer the radius.

@section{method}
  angle
Answer the angle in radians

@section{method}
  phase
Answer the angle in radians

@section{method}
  real
Answer the real part.

@section{method}
  imag
Answer the imaginary part.

@section{method}
  asComplex
Convert to Complex

@section{method}
  asPoint
Convert to Point



