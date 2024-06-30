#lang scribble/manual
@(require (for-label racket))

@title{DelTapWr}
@section{categories}
  UGens>Buffer, UGens>Delays
 Write to a buffer for a DelTapRd UGen@section{related}
  Classes/DelTapRd

@section{description}

Write to a buffer for a link::Classes/DelTapRd:: UGen.

@section{note}
  If you run a 
@racketblock[DelTapRd.ar:: and a ]

@racketblock[DelTapWr.ar:: in tandem, keep in mind that they read and write in blocks
equal to the server's block size. If the delay time is greater than the buffer size minus a block, the write and read
heads might interfere in unintended ways. Use a slightly larger buffer if this happens. ::

]
@section{classmethods}
 
@section{private}
  categories

@section{method}
  ar, kr
@section{argument}
  buffer
the buffer to write signal into. Max delay time is based on buffer size.
@section{argument}
  in
the signal to write to the buffer.
@section{returns}
 
phase - DelTapWr outputs its current sample value for use in the phase argument in DelTapRd

@section{examples}
 

@racketblock[
// a Buffer for the UGens to use, one second at the current sample rate
b = Buffer.alloc(s, s.sampleRate * 1, 1);

// write a signal into a delay, tap it at multiple times
SynthDef(\test, {arg buffer;
	var src, tapPhase, tap1, tap2, tap3;
	src = WhiteNoise.ar(0.2) * Decay.kr(Dust.kr(3), 0.2);
	tapPhase = DelTapWr.ar(buffer, src);
	#tap1, tap2, tap3 = DelTapRd.ar(buffer, tapPhase,
		[0.2, 0.27, 0.303],  	// tap times
		1,  					// no interp
		[1.0, 0.4, 0.2] 		// muls for each tap
		);
	Out.ar(0, [src + tap2, tap1 + tap3])
	}).add;

x = Synth(\test, [\buffer, b]);
x.free;
b.free;
::

]

@racketblock[
// a Buffer for the UGens to use
b = Buffer.alloc(s, 44100, 1);

// write a signal into a delay, tap it at multiple times
SynthDef(\write, {arg buffer, cout;
	var src, tapPhase, tap1, tap2, tap3;
	src = WhiteNoise.ar(0.2) * Decay.kr(Dust.kr(3), 0.7);
	tapPhase = DelTapWr.ar(buffer, src);
	Out.kr(cout, tapPhase);
	}).add;

SynthDef(\readFilt, {arg buffer, cin;
	var phase, src, filt;
	phase = In.kr(cin);
	src = DelTapRd.ar(buffer, phase, [0.01, 0.2]);
	filt = BPF.ar(src, 880, 0.01) * 10;
	Out.ar(0, filt);
	}).add;

c = Bus.control;

x = Synth(\write, [\buffer, b, \cout, c]);
y = Synth(\readFilt, [\buffer, b, \cin, c]);

x.free;
b.free;
::

]

