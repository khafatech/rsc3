#lang scribble/manual
@(require (for-label racket))

@title{MostChange}
 Output most changed.@section{related}
  Classes/LeastChange, Classes/LastValue
@section{categories}
   UGens>Maths


@section{description}


Given two inputs 
@racketblock[a:: and ]

@racketblock[b::, let ]

@racketblock[da[t] = abs(a[t] - a[t - 1]):: and ]

@racketblock[db[t] = abs(b[t] - b[t - 1])::. Output ]

@racketblock[a[t]:: if ]

@racketblock[da[t]:: is larger, and output ]

@racketblock[b[t]:: if ]

@racketblock[db[t]:: is larger. If ]

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


@section{Examples}
 


@racketblock[

//doesn't work yet.

d = SynthDef("help-MostChange", { arg amp=1.0;
	var out, in1, in2;
	in1 = LFNoise1.ar(800, amp);
	in2 = SinOsc.ar(800);
	out = MostChange.ar(in1, in2) * 0.1;
	Out.ar(0, out)
}).play;

d.set(\amp, 0.1);
d.set(\amp, 0);
d.set(\amp, 3);
d.free;

the control that changed most is used for output:

d = SynthDef("help-MostChange", { arg freq=440;
	var out, internalFreq;
	internalFreq = LFNoise0.ar(0.3, 300, 800);
	out = SinOsc.ar(
			MostChange.kr(freq, internalFreq)
	);
	Out.ar(0, out * 0.1)
}).play;

d.set(\freq, 800);
d.set(\freq, 800); //nothing changed
d.set(\freq, 900);
d.free;

::

]


