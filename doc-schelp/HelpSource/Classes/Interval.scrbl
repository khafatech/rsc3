#lang scribble/manual
@(require (for-label racket))

@title{Interval}
range of integers@section{categories}
  Math

@section{description}

An Interval is a range of integers from a starting value to an ending value by some step value.

@section{CLASSMETHODS}
 

@section{method}
 new
Create a new Interval.

@racketblock[
Interval(10, 30, 4);
10 to: 30; // the message to creates an interval with step 1
::

]
@section{INSTANCEMETHODS}
 

@section{method}
 start
The starting value of the interval.

@section{method}
 end
The ending value of the interval.

@section{method}
 step
The step value of the interval.

@section{method}
 size
Return the number of items in the interval.

@racketblock[
Interval(10, 30, 4).size.postln;
::

]
@section{method}
 at
Return the indexed item in the interval.

@racketblock[
Interval(10, 30, 4).at(3).postln;
::

]
@section{method}
 do
Evaluates function for each item in the interval. The function is passed two arguments, the item and an integer index.

@racketblock[
Interval(10, 30, 4).do({ arg item, i; item.postln });
::
]


