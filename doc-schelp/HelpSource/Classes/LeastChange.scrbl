#lang scribble/manual
@(require (for-label racket))

@title{LeastChange}
 Output least changed@section{related}
  Classes/MostChange, Classes/LastValue
@section{categories}
   UGens>Maths


@section{description}


Given two inputs 
@racketblock[a:: and ]

@racketblock[b::, let ]

@racketblock[da[t] = abs(a[t] - a[t - 1]):: and ]

@racketblock[db[t] = abs(b[t] - b[t - 1])::. Output ]

@racketblock[a[t]:: if ]

@racketblock[da[t]:: is smaller, and output ]

@racketblock[b[t]:: if ]

@racketblock[db[t]:: is smaller. If ]

@racketblock[da[t] == db[t]::, use whichever input was used last (assume ]

@racketblock[a:: for the first sample of output).


]
@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 a

Input signal A.


@section{argument}
 b

Input signal B.

@section{examples}
 


@racketblock[
(
d = { arg amp=1.0;
	var in1, in2;
	in1 = LFNoise0.ar(800, amp);
	in2 = SinOsc.ar(800);
	LeastChange.ar(in1, in2) * 0.1;
}.play;
)

d.set(\amp, 0.1);
d.set(\amp, 0);
d.set(\amp, 3);
d.free;
::

the control that changed least is used as output:

]

@racketblock[
(
d = { arg freq=440;
	var internalFreq;
	internalFreq = LFNoise0.ar(0.3, 300, 800);
	SinOsc.ar(
		LeastChange.kr(freq, internalFreq) // two sources of change: one external, one internal
	) * 0.1
}.play
);

d.set(\freq, 800);
d.set(\freq, 900);
d.free;
::

]


