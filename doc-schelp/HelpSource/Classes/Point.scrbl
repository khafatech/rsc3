#lang scribble/manual
@(require (for-label racket))

@title{Point}
 Cartesian point@section{related}
  Classes/Polar, Classes/Complex
@section{categories}
  Geometry

@section{description}

Defines a point on the Cartesian plane.

@section{classmethods}
 

@section{method}
 new
Defines a new point.

@section{instancemethods}
 

@section{subsection}
  Accessing

@section{method}
 x
Get or set the x coordinate value.

@section{method}
 y
Get or set the y coordinate value.

@section{method}
 set
Sets the point x and y values.

@section{subsection}
  Testing

@section{method}
 ==
Answers a Boolean whether the receiver equals the argument.

@section{method}
 hash
Returns a hash value for the receiver.

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
 translate
Addition by a Point.

@section{method}
 scale
Multiplication by a Point.

@section{method}
 rotate
Rotation about the origin by the angle given in radians.

@section{method}
 abs
Absolute value of the point.

@section{method}
 rho
Return the polar coordinate radius of the receiver.

@section{method}
 theta
Return the polar coordinate angle of the receiver.

@section{method}
 dist
Return the distance from the receiver to aPoint.

@section{method}
 transpose
Return a Point whose x and y coordinates are swapped.

@section{method}
 round
Round the coordinate values to a multiple of quantum.

@section{method}
 trunc
Truncate the coordinate values to a multiple of quantum.

@section{subsection}
  Conversion

@section{method}
 asPoint
Returns the receiver.

@section{method}
 asComplex
Returns a complex number with x as the real part and y as the imaginary part.

@section{method}
 asString
Return a string representing the receiver.


