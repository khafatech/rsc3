#lang scribble/manual
@(require (for-label racket))

@title{MulAdd}
@section{categories}
  UGens>Maths
 Multiply and add to a signal
@section{description}

Multiplies the signal by mul and adds add. This UGen is very efficient (it performs various optimisation checks, for example). It is used very heavily throughout SuperCollider to perform multiply and add operations on the server; in fact it is  what "really" performs the mul and add arguments found in many UGens.

See also the discussion of mul and add arguments in the link::Classes/UGen:: help file.

@section{classmethods}
 
@section{private}
  new1

@section{method}
  new
@section{argument}
  in
input signal
@section{argument}
  mul
multiply with this value
@section{argument}
  add
add this value

@section{discussion}
 
Same as:

@racketblock[
in.madd(mul, add)
::

]
@section{instancemethods}
 
@section{private}
  init

@section{Examples}
 

@racketblock[
s.boot;

// The mul and add arguments of SinOsc themselves use MulAdd!
// These two examples will create precisely the same synth graph:
x = { SinOsc.ar(440, 0, 0.1, 0.05) }.play(s);
x.trace; // You should see a "MulAdd" in the trace
x.free;

x = { MulAdd(SinOsc.ar(440, 0), 0.1, 0.05) }.play(s);
x.trace;
x.free;

// In fact this will produce the same graph too - the separate multiply and add are optimised into one MulAdd
x = { SinOsc.ar(440, 0) * 0.1 + 0.05 }.play(s);
x.trace;
x.free;
::
(Note: the "trace" message is described in the helpfile for link::Classes/Node::.)
]


