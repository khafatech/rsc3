#lang scribble/manual
@(require (for-label racket))

@title{BufWr}
 Buffer writing oscillator.@section{related}
  Classes/BufRd
@section{categories}
   UGens>Buffer

@section{description}

Write to a buffer at an index.

@section{note}
  BufWr (in difference to  link::Classes/BufRd:: ) does not do multichannel expansion, because input is an array. ::

@section{classmethods}
 
@section{private}
  categories

@section{method}
 ar, kr

@section{argument}
 inputArray
Input UGens (channelArray).

@section{argument}
 bufnum
The index of the buffer to use.

@section{argument}
 phase
Modulateable index into the buffer (has to be audio rate).
@section{Warning}
  The phase argument only offers precision for addressing 2**24 samples (about 6.3 minutes at 44100Hz) ::

@section{argument}
 loop
1 means true, 0 means false. This is modulateable.

@section{instancemethods}
 
@section{private}
  checkInputs

@section{Examples}
 


@racketblock[

(
// allocate a buffer for writinig into
s = Server.local;
s.sendMsg("/b_alloc", 0, 44100 * 2);
)


//write into the buffer with a BufWr
(
y = { arg rate=1;
	var in;
	in = SinOsc.ar(LFNoise1.kr(2, 300, 400), 0, 0.1);
	BufWr.ar(in, 0, Phasor.ar(0, BufRateScale.kr(0) * rate, 0, BufFrames.kr(0)));
	0.0 //quiet
}.play;
)

//read it with a BufRd
(
x = { arg rate=1;
	BufRd.ar(1, 0, Phasor.ar(0, BufRateScale.kr(0) * rate, 0, BufFrames.kr(0)))
}.play(s);
)



x.set(\rate, 5);
y.set(\rate, 2.0.rand);
x.set(\rate, 2);

::

]


