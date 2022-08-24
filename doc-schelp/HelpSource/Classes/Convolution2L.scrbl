#lang scribble/manual
@(require (for-label racket))

@title{Convolution2L}
 Real-time convolver with linear interpolation@section{related}
  Classes/Convolution, Classes/Convolution2, Classes/Convolution3, Classes/StereoConvolution2L
@section{categories}
   UGens>FFT, UGens>Convolution

@section{description}

Strict convolution with fixed kernel which can be updated using a trigger signal. There is a linear crossfade between the buffers upon change.

See emphasis:: Steven W Smith, The Scientist and Engineer's Guide to Digital Signal Processing: chapter 18:: -
http://www.dspguide.com/ch18.htm

@section{classmethods}
 
@section{private}
  categories

@section{method}
 ar

@section{argument}
 in
processing target

@section{argument}
 kernel
buffer index for the fixed kernel, may be modulated in combination with the trigger

@section{argument}
 trigger
update the kernel on a change from <=0 to >0

@section{argument}
 framesize
size of FFT frame, must be a power of two (512, 1024, 2048, 4096 are standard choices).
Convolution uses twice this number internally.
Note that the convolution gets progressively more expensive to run for higher powers!
The maximum value you can use is 2^16=16384.
(This upper limit is half of "SC_FFT_MAXSIZE" defined in the SC source code.)
Larger convolutions than this can be done using link::Classes/PartConv::.


@section{argument}
 crossfade
The number of periods over which a crossfade is made. The default is 1. This must be an integer.

@section{argument}
 mul

@section{argument}
 add

@section{Examples}
 


@racketblock[
(// allocate three buffers
b = Buffer.alloc(s, 2048);
c = Buffer.alloc(s, 2048);
d = Buffer.alloc(s, 2048);

b.zero;
c.zero;
d.zero;
)

(
50.do({ |it| c.set(20*it+10, 1.0.rand); });
3.do({ |it| b.set(400*it+100, 1); });
20.do({ |it| d.set(40*it+20, 1); });
)
::
]

@racketblock[
(
SynthDef(\conv_test, { arg kernel, t_trig=0;
	var input;

	input=Impulse.ar(1);

	// must have power of two framesize
	Out.ar(0, Convolution2L.ar(input, kernel, t_trig, 2048, 1, 0.5));
}).add
)

x = Synth(\conv_test, [\kernel, b]);

// changing the buffer number:
x.set(\kernel, c);
x.set(\t_trig, 1); // after this trigger, the change will take effect.
x.set(\kernel, d);
x.set(\t_trig, 1); // after this trigger, the change will take effect.

d.zero;
40.do({ |it| d.set(20*it+10, 1); });// changing the buffers' contents
x.set(\t_trig, 1); // after this trigger, the change will take effect.

x.set(\kernel, b);
x.set(\t_trig, 1); // after this trigger, the change will take effect.

x.free;
::

]

@racketblock[
// longer crossfade
(
SynthDef( \conv_test2, { arg kernel, t_trig=0;
	var input;

	input=Impulse.ar(1);

	// must have power of two framesize
	Out.ar(0, Convolution2L.ar(input, kernel, t_trig, 2048, 5, 0.5));
}).add
)

x = Synth(\conv_test2, [\kernel, b]);

// changing the buffer number:
x.set(\kernel, c);
x.set(\t_trig, 1); // after this trigger, the change will take effect.
x.set(\kernel, d);
x.set(\t_trig, 1); // after this trigger, the change will take effect.

d.zero;
40.do({ |it| d.set(20*it+10, 1); });// changing the buffers' contents
x.set(\t_trig, 1); // after this trigger, the change will take effect.

x.set(\kernel, b);
x.set(\t_trig, 1); // after this trigger, the change will take effect.

x.free;
::

]

@racketblock[
// next example

b = Buffer.read(s, Platform.resourceDir +/+ "sounds/a11wlk01.wav");

(
	{ var input, kernel;

	input= SoundIn.ar(0);

	// must have power of two framesize
	Convolution2L.ar(input, b, 0, 512, 1, 0.5);
	}.play;

)
::

]

@racketblock[
// another example

(
// must have power of two framesize- FFT size will be sorted by Convolution2 to be double this
// maximum is currently a=8192 for FFT of size 16384
a=2048;
// kernel buffer
g = Buffer.alloc(s, a, 1);
)

(
g.set(0, 1.0);
100.do({arg i; g.set(a.rand, (i+1).reciprocal)});
)

(
// random impulse response

	{
	var input, inputAmp, threshhold, gate;

	input = SoundIn.ar(0);
	inputAmp = Amplitude.kr(input);
	threshhold = 0.02;	// noise gating threshold
	gate = Lag.kr(inputAmp > threshhold, 0.01);

	Convolution2L.ar(input*gate, g, 0, a, 1, 0.5);
	}.play;

)
::

]

@racketblock[
// one last example
(
b = Buffer.alloc(s, 512, 1);
b.sine1(1.0/[1, 2, 3, 4, 5, 6], true, true, true);
)

(
	{ var input, kernel;

	input=SoundIn.ar(0);

	// must have power of two framesize
	Convolution2L.ar(input, b, 0, 512, 1, 0.5);
	}.play;

)
::

]


