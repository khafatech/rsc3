#lang scribble/manual
@(require (for-label racket))

@title{StereoConvolution2L}
 Stereo real-time convolver with linear interpolation@section{categories}
  UGens>FFT, UGens>Convolution
@section{related}
  Classes/Convolution, Classes/Convolution2L

@section{description}

Strict convolution with fixed kernel which can be updated using a trigger signal. There is a linear crossfade between the buffers upon change.

Like link::Classes/Convolution2L::, but convolves with two buffers and outputs a stereo signal. This saves one FFT transformation per period, as compared to using two copies of link::Classes/Convolution2L::.

Useful applications could include stereo reverberation or HRTF convolution.

See Steven W Smith, The Scientist and Engineer's Guide to Digital Signal Processing, chapter 18: link::http://www.dspguide.com/ch18.htm::

@section{classmethods}
 
@section{method}
  ar

@section{argument}
  in
processing target.
@section{argument}
  kernelL
buffer index for the fixed kernel of the left channel, may be modulated in combination with the trigger.
@section{argument}
  kernelR
buffer index for the fixed kernel of the right channel, may be modulated in combination with the trigger.
@section{argument}
  trigger
update the kernel on a change from <= 0 to > 0.
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

@section{examples}
 

@racketblock[
(
//allocate three buffers
b = Buffer.alloc(s, 2048);
c = Buffer.alloc(s, 2048);
d = Buffer.alloc(s, 2048);

b.zero;
c.zero;
d.zero;
)

(
50.do({ |it| c.set(20 * it + 10, 1.0.rand); });
3.do({ |it| b.set(400 * it + 100, 1); });
20.do({ |it| d.set(40 * it + 20, 1); });
)


(
SynthDef(\conv_test, { arg kernel1, kernel2, t_trig = 0;
	var input;

	input = Impulse.ar(1);

	// must have power of two framesize
	Out.ar(0, StereoConvolution2L.ar(input, kernel1, kernel2, t_trig, 2048, 1, 0.5));
}).add

)


x = Synth(\conv_test, [\kernel1, b, \kernel2, c]);

// changing the buffer number:
x.set(\kernel1,d);
x.set(\t_trig,1); // after this trigger, the change will take effect.
x.set(\kernel2,d);
x.set(\t_trig,1); // after this trigger, the change will take effect.

d.zero;
40.do({ |it| d.set(20 * it + 10, 1); });// changing the buffers' contents
x.set(\t_trig, 1); // after this trigger, the change will take effect.

x.set(\kernel1, b);
x.set(\t_trig, 1); // after this trigger, the change will take effect.

x.free;
::
]


