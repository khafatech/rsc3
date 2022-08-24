#lang scribble/manual
@(require (for-label racket))

@title{DetectIndex}
 Search a buffer for a value@section{categories}
  UGens>Buffer

@section{description}

Search a buffer for a value.

@section{classmethods}
 
@section{method}
  ar, kr

@section{argument}
  bufnum
index of the buffer
@section{argument}
  in
the input signal.
@section{returns}
 
index

@section{examples}
 

@racketblock[
(
var max = 300;
t = Array.series(max, 0, 1).curdle(0.06).scramble.flat;
b = Buffer(s, t.size, 1);

// alloc and set the values
s.listSendMsg( b.allocMsg( b.setnMsg(0, t) ) );


{
	var index, in, out, f0, fdiff;
	var bufnum = b;
	var input;
	input = MouseX.kr(0, max).round(1); // round to precision
	index = DetectIndex.kr(bufnum, input);
	index.poll;
	SinOsc.ar(index.linexp(0, max, 200, 700)) * 0.1
}.play;
)

b.free;
::

]


