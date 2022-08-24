#lang scribble/manual
@(require (for-label racket))

@title{Stepper}
 Pulse counter.@section{related}
  Classes/PulseCount
@section{categories}
   UGens>Triggers


@section{description}


Each trigger increments a counter which is output as a signal. The
counter wraps between  
@racketblock[min::  and
]

@racketblock[max:: .


]
@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 trig

The trigger. Trigger can be any signal. A trigger happens when
the signal changes from non-positive to positive.


@section{argument}
 reset

Resets the counter to

@racketblock[resetval::  when
triggered.


]
@section{argument}
 min

Minimum value of the counter.


@section{argument}
 max

Maximum value of the counter.


@section{argument}
 step

Step value each trigger. May be negative.


@section{argument}
 resetval

Value to which the counter is reset when it receives a reset
trigger. If nil, then this is patched to


@racketblock[min:: .


]
@section{Examples}
 


@racketblock[

SynthDef("help-Stepper",{ arg out=0;
	Out.ar(out,
		SinOsc.ar(
			Stepper.kr(Impulse.kr(10), 0, 4, 16, 1) * 100,
			0, 0.05
		)
	)
}).play;

SynthDef("help-Stepper",{ arg out=0;
	Out.ar(out,
		SinOsc.ar(
			Stepper.kr(Impulse.kr(10), 0, 4, 16, -3) * 100,
			0, 0.05
		)
	)
}).play;

SynthDef("help-Stepper",{ arg out=0;
	Out.ar(out,
		SinOsc.ar(
			Stepper.kr(Impulse.kr(10), 0, 4, 16, 4) * 100,
			0, 0.05
		)
	)
}).play;


///////////////////////////////////////////////////////////////////////////////////
//
// Using Stepper and BufRd for sequencing
//

s.boot;

s.sendMsg(\b_alloc, 10, 128);

m = #[0,3,5,7,10];

a = ({rrand(0,15)}.dup(16).degreeToKey(m) + 36).midicps;
s.performList(\sendMsg, \b_setn, 10, 0, a.size, a);

(
SynthDef(\stepper, {
	var rate, clock, index, freq, ffreq, env, out, rev, lfo;

	rate = MouseX.kr(1,5,1);
	clock = Impulse.kr(rate);
	env = Decay2.kr(clock, 0.002, 2.5);
	index = Stepper.kr(clock, 0, 0, 15, 1, 0);
	freq = BufRd.kr(1, 10, index, 1, 1);
	freq = Lag2.kr(freq) + [0,0.3];
	ffreq = MouseY.kr(80,1600,1) * (env * 4 + 2);
	out = Mix.ar(LFPulse.ar(freq * [1, 3/2, 2], 0, 0.3));
	out = RLPF.ar(out, ffreq, 0.3, env);
	out = RLPF.ar(out, ffreq, 0.3, env);
	out = out * 0.02;

	// echo
	out = CombL.ar(out, 1, 0.66/rate, 2, 0.8, out);

	// reverb
	rev = out;
	5.do { rev = AllpassN.ar(rev, 0.05, {0.05.rand}.dup, rrand(1.5,2.0)) };
	out = out + (0.3 * rev);

	out = LeakDC.ar(out);

	// flanger
	lfo = SinOsc.kr(0.2, [0,0.5pi], 0.0024, 0.0025);
	1.do { out = DelayL.ar(out, 0.1, lfo, 1, out) };

	// slight bass emphasis
	out = OnePole.ar(out, 0.9);

	Out.ar(0, out);

}).add;
)

s.sendMsg(\s_new, \stepper, 1000, 0, 0);

a = ({rrand(0,15)}.dup(16).degreeToKey(m) + 38).midicps;
s.performList(\sendMsg, \b_setn, 10, 0, a.size, a);

a = a * 2.midiratio; // transpose up 2 semitones
s.performList(\sendMsg, \b_setn, 10, 0, a.size, a);


(
a = [ 97.999, 195.998, 523.251, 466.164, 195.998, 233.082, 87.307, 391.995, 87.307, 261.626, 195.998, 77.782, 233.082, 195.998, 97.999, 155.563 ];
s.performList(\sendMsg, \b_setn, 10, 0, a.size, a);
)

s.sendMsg(\n_free, 1000);

::

]


