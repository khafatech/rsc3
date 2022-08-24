#lang scribble/manual
@(require (for-label racket))

@title{CheckBadValues}
 Test for infinity, not-a-number, and denormals@section{categories}
  UGens>Info

@section{description}

This link::Classes/UGen:: tests for infinity, NaN (not a number), and denormals. If one of these is found, it posts a warning. Its output is as follows: 0 = a normal float, 1 = NaN, 2 = infinity, and 3 = a denormal.

@section{classmethods}
 

@section{method}
  ar, kr
@section{argument}
  in
the link::Classes/UGen:: whose output is to be tested.
@section{argument}
  id
an id number to identify this link::Classes/UGen::. The default is 0.
@section{argument}
  post
One of three post modes:

@section{list}
 
## 0 = no posting;
## 1 = post a line for every bad value;
## 2 = post a line only when the floating-point classification changes (e.g., normal -> NaN and vice versa)
::

The default post mode is 2. Post mode 1 is retained for backward compatibility; be aware that it generates a large amount of output.

@section{examples}
 

@racketblock[
{ CheckBadValues.kr(SinOsc.ar); 0}.play // nothing wrong here

{ CheckBadValues.kr(1 / 0, 1).poll; 0 }.play // check infinity

x = { arg test = -1; CheckBadValues.kr(test); 0 }.play // check NaN
x.set(\test, -1.sqrt);

// don't post, but do something with the output
(
x = { arg freq = 440;
	var good;
	good = BinaryOpUGen('==', CheckBadValues.kr(freq, 0, 0), 0);
	SinOsc.ar(freq, 0, 0.1) * good // silence the output if freq is bad
}.play;
)
x.set(\freq, -1.sqrt);

// the UGen method checkBadValues passes through the input for quick testing
{ SinOsc.ar(440, 0, 0.1).checkBadValues }.play
::
]


