#lang scribble/manual
@(require (for-label racket))

@title{Rect}
 Rectangle@section{categories}
  Geometry

@section{ClassMethods}
 

@section{method}
 new
Return a new Rect with the given upper left corner and dimensions.

@section{method}
 newSides
Return a new Rect with the given boundaries.

@section{method}
 fromPoints
Return a new Rect defined by the given Points.

@section{InstanceMethods}
 

@section{method}
 left
Get or set the value of the boundary.

@section{method}
 top
Get or set the value of the boundary.

@section{method}
 right
Get the value of the boundary.

@section{method}
 bottom
Get the value of the boundary.

@section{method}
 set
Set the boundaries to the given values.

@section{method}
 setExtent
Set the dimensions.

@section{method}
 width
Set or get the width.

@section{method}
 height
Set or get the height.

@section{method}
 origin
Return the upper left corner as a Point.

@section{method}
 extent
Return a Point whose x value is the height and whose y value is the width.

@section{method}
 leftTop
Return the upper left corner as a Point.

@section{method}
 rightTop
Return the upper right corner as a Point.

@section{method}
 leftBottom
Return the lower left corner as a Point.

@section{method}
 rightBottom
Return the lower right corner as a Point.

@section{method}
 moveBy
Returns a new Rect which is offset by x and y.

@section{method}
 moveTo
Returns a new Rect whose upper left corner is moved to (x, y).

@section{method}
 moveToPoint
Returns a new Rect whose upper left corner is moved to aPoint.

@section{method}
 resizeBy
Returns a new Rect whose dimensions have been changed by (x, y).

@section{method}
 resizeTo
Returns a new Rect whose dimensions are (x, y).

@section{method}
 insetBy
Returns a new Rect whose boundaries have been inset by (x, y). If only one argument is supplied, it will be used for both x and y.

@section{method}
 insetAll
Returns a new Rect whose boundaries have been inset by the given amounts.

@section{method}
 contains
Answers whether aPoint is in the receiver.

@section{method}
 union, |
Returns a new Rect which contains the receiver and aRect.

@section{method}
 sect, &
Returns a new Rect which is the intersection of the receiver and aRect.



