#lang scribble/manual
@(require (for-label racket))

@title{Thunk}
unevaluated value@section{categories}
 Core>Kernel

@section{description}


Thunk, "past tense of think", can be used 	when a calculation may, or may not have to be performed at a later point in time, and its value is needed several times. This is an example of lazy evaluation, and can be used to avoid unnecessary calculations and to make state immutable.

@section{classMethods}
 

@section{method}
 new

@section{argument}
 function
some function that returns the desired value

@section{instanceMethods}
 

@section{method}
 value

return the value. If calculation is done, use previous value, otherwise do calculation.

@section{examples}
 


@racketblock[
// so for example, random values will result in a single instance:
a = Thunk({ \done.postln; rrand(2.0, 8.0) });
a.value; // posts "done"
a.value;
::

]

@racketblock[
// it is an AbstractFunction, so one can use it for math operations:

a = Thunk({ rrand(2.0, 8.0) });
b = a * 5 / (a - 1);
b.value;
::

]

@racketblock[
// lazy evaluation

a = Thunk({ \done1.postln; Array.fill(10000, { |i| i + 6 % 5 * i / 2 }) }); // some calculation.
b = Thunk({ \done2.postln;Array.fill(10000, { |i| i + 5 % 6 * i / 3 }) });// some other calculation.
c = [a, b].choose + 700;
(c * c * c).value; // calculation happens here, and only once.

// compare to a function:

a = { \done1.postln; Array.fill(10000, { |i| i + 6 % 5 * i / 2 }) }; // some calculation.
b = { \done2.postln;Array.fill(10000, { |i| i + 5 % 6 * i / 3 }) };// some other calculation.
c = [a, b].choose + 700;
(c * c * c).value; // calculation happens here, but 3 times (for each c)
::

]


