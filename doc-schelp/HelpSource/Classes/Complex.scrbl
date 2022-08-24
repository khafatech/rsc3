#lang scribble/manual
@(require (for-label racket))

@title{Complex}
 complex number@section{categories}
  Math
@section{related}
 Classes/Polar, Classes/SimpleNumber, Classes/Float, Classes/Integer

@section{description}

A class representing complex numbers.
Note that this is a simplified representation of a complex number, which does not implement the full mathematical notion of a complex number.

@section{CLASSMETHODS}
 
@section{method}
  new
Create a new complex number with the given real and imaginary parts.
@section{argument}
  real
the real part
@section{argument}
  imag
the imaginary part

@section{returns}
  a new instance of Complex.
@section{discussion}
 

@racketblock[
a = Complex(2, 5);
a.real;
a.imag;
::

]
@section{INSTANCEMETHODS}
 

@section{subsection}
  math support
@section{method}
  real
The real part of the number.

@section{method}
  imag
The imaginary part of the number.

@section{method}
  conjugate
the complex conjugate.
@section{discussion}
 

@racketblock[
Complex(2, 9).conjugate
::

]
@section{method}
  +
Complex addition.
@section{discussion}
 

@racketblock[
Complex(2, 9) + Complex(-6, 2)
::

]
@section{method}
  -
Complex subtraction
@section{discussion}
 

@racketblock[
Complex(2, 9) - Complex(-6, 2)
::

]
@section{method}
  *
Complex multiplication
@section{discussion}
 

@racketblock[
Complex(2, 9) * Complex(-6, 2)
::

]
@section{method}
  /
Complex division.
@section{discussion}
 

@racketblock[
Complex(2, 9) / Complex(-6, 2)
::

]
@section{method}
  exp
Complex exponentiation with base e.
@section{discussion}
 

@racketblock[
exp(Complex(2, 9))
::
]

@racketblock[
exp(Complex(0, pi)) == -1 // Euler's formula: true
::

]
@section{method}
  squared
Complex self multiplication.
@section{discussion}
 

@racketblock[
squared(Complex(2, 1))
::

]
@section{method}
  cubed
complex triple self multiplication.
@section{discussion}
 

@racketblock[
cubed(Complex(2, 1))
::

]
@section{method}
  **, pow
Complex exponentiation
@section{discussion}
 
not implemented for all combinations - some are mathematically ambiguous.

@racketblock[
Complex(0, 2) ** 6
::
]

@racketblock[
2.3 ** Complex(0, 2)
::
]

@racketblock[
Complex(2, 9) ** 1.2 // not defined
::


]
@section{method}
  <
the comparison of just the real parts.
@section{discussion}
 

@racketblock[
Complex(2, 9) < Complex(5, 1);
::

]
@section{method}
  ==
the comparison assuming that the reals (floats) are fully embedded in the complex numbers
@section{discussion}
 

@racketblock[
Complex(1, 0) == 1;
Complex(1, 5) == Complex(1, 5);
::

]
@section{method}
  neg
negation of both parts
@section{discussion}
 

@racketblock[
Complex(2, 9).neg
::

]
@section{method}
  abs
the absolute value of a complex number is its magnitude.
@section{discussion}
 

@racketblock[
Complex(3, 4).abs
::

]
@section{method}
  magnitude
distance to the origin.

@section{method}
  magnitudeApx

@section{method}
  rho
the distance to the origin.

@section{method}
  angle, phase, theta
the angle in radians.


@section{subsection}
  conversion
@section{method}
  asPoint
Convert to a link::Classes/Point::.

@section{method}
  asPolar
Convert to a Polar

@section{method}
  asInteger
real part as link::Classes/Integer::.

@section{method}
  asFloat
real part as link::Classes/Float::.

@section{method}
  asComplex
returns this


@section{subsection}
  misc
@section{method}
  coerce
@section{method}
  hash
a hash value
@section{method}
  printOn
print this on given stream
@section{method}
  performBinaryOpOnSignal
@section{method}
  performBinaryOpOnComplex
@section{method}
  performBinaryOpOnSimpleNumber
@section{method}
  performBinaryOpOnUGen




@section{EXAMPLES}
 

Basic example:

@racketblock[
a = Complex(0, 1);
a * a; // returns Complex(-1, 0);
::

Julia set approximation:
]

@racketblock[
f = { |z| z * z + Complex(0.70176, 0.3842) };

(
var n = 80, xs = 400, ys = 400, dx = xs / n, dy = ys / n, zoom = 3, offset = -0.5;
var field = { |x| { |y| Complex(x / n + offset * zoom, y / n + offset * zoom) } ! n } ! n;

w = Window("Julia set", bounds:Rect(200, 200, xs, ys)).front;
w.view.background_(Color.black);
w.drawFunc = {
	n.do { |x|
		n.do { |y|
			var z = field[x][y];
			z = f.(z);
			field[x][y] = z;
			Pen.color = Color.gray(z.rho.linlin(-100, 100, 1, 0));
 			Pen.addRect(
				Rect(x * dx, y * dy, dx, dy)
			);
			Pen.fill
		}
	}
};

fork({ 6.do { w.refresh; 2.wait } }, AppClock)
)
::
]


