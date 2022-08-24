#lang scribble/manual
@(require (for-label racket))

@title{Array2D}
two-dimensional array@section{related}
 Classes/Array
@section{categories}
 Collections>Ordered

@section{description}

Represents a two-dimensional array of data. The number of rows and columns is fixed.

@section{note}
  It is possible to implement a similar behaviour using an "array-of-arrays" - see the examples towards the bottom of this page for comparison.::

@section{CLASSMETHODS}
 

@section{method}
 new
Create an array of the specified size.

@racketblock[
a = Array2D.new(3,4);
a[2,2] = 1;
a.postln
::

]
@section{method}
 fromArray
Build an Array2D from the supplied array.

@racketblock[
a = Array2D.fromArray(3,4, [9,8,7,6,5,4,3,2,1,2,3,4]);
a[2,2] = 1;
a.postln
::

]
@section{INSTANCEMETHODS}
 

@section{private}
 printOn, storeOn

@section{method}
 at
Get a value from the array.

@racketblock[
a.at(2,3);
a[2,3];
::

]
@section{method}
 put
Put a value into the array.

@racketblock[
a.put(2,3, 72);
a[2,3] = 72;
::

]
@section{method}
 colsDo
Iterate over the columns. Each column will be passed to strong::func:: in turn.

@racketblock[
a.colsDo(_.postln);
::

]
@section{method}
 rowsDo
Iterate over the rows. Each row will be passed to strong::func:: in turn.

@racketblock[
a.rowsDo(_.postln);
::

]
@section{method}
 colAt
Retrieve a single column.

@racketblock[
a.colAt(2);
::

]
@section{method}
 rowAt
Retrieve a single row.

@racketblock[
a.rowAt(2);
::

]
@section{method}
 asArray
Return a flat array containing the elements.

@racketblock[
a.postln;
a.asArray.postln;
::
]
@section{returns}
  link::Classes/Array::

@section{EXAMPLES}
 


@racketblock[
// "a" is an array-of-arrays
a = { { 100.0.rand }.dup(100) }.dup(100);
// "b" is an equivalent Array2D, made using the "fromArray" class method
b = Array2D.fromArray(100,100, a.flat);

// Accessing
a[15][22]
b[15, 22]

// Speed comparison 1: random access
bench { 100.do(a[100.rand][100.rand]) }
bench { 100.do(b[100.rand, 100.rand]) }

// Speed comparison 2: iteration
bench { 100.do(a.do { |row| row.do { |item| item * 2 } }) }
bench { 100.do(b.do { |item| item * 2 }) }
::
]


