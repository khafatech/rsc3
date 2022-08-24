#lang scribble/manual
@(require (for-label racket))

@title{MidEQ}
 Parametric filter.@section{categories}
   UGens>Filters>Linear


@section{description}


Attenuates or boosts a frequency band.


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 in

The input signal.


@section{argument}
 freq

Center frequency of the band in Hertz.


@section{argument}
 rq

The reciprocal of Q (bandwidth / cutoffFreq).


@section{argument}
 db

Amount of boost (db > 0) or attenuation (db < 0) of the
frequency band.


@section{argument}
 mul

Output will be multiplied by this value.


@section{argument}
 add

This value will be added to the output.


@section{Examples}
 


@racketblock[

// mixer parametric eq as wahwah
{ MidEQ.ar(Saw.ar(200,0.2), FSinOsc.kr(1, 0, 24, 84).midicps, 0.3, 12) }.play


// notch filter
(
{ var in;
  in = PinkNoise.ar(0.2) + SinOsc.ar(600, 0, 0.1);
  MidEQ.ar(in, SinOsc.kr(0.2, 0.5pi) * 2 + 600, 0.01, -24)
}.play
)

/////
// first start the synth
(
x = {| freq=400, db=0, rq=0.1 |
	 var in;
	 in =  SinOsc.ar([ 400, 420 ], 0, 0.4);
	 MidEQ.ar(in, freq, Lag.kr(rq, 0.3), db)
	}.play
)

// now play with its parameters to hear how the filter affects two frequencies
// that are very close to each other

x.set(\db, -12)
x.set(\rq, 0.1)
x.set(\rq, 0.03)
x.set(\freq, 410)
x.set(\freq, 420)
x.set(\freq, 400)
x.set(\freq, 500)

::
]


