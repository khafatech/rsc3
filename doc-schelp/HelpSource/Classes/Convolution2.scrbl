#lang scribble/manual
@(require (for-label racket))

@title{Convolution2}
 Real-time fixed kernel convolver.@section{related}
  Classes/Convolution, Classes/Convolution2L, Classes/Convolution3
@section{categories}
   UGens>FFT, UGens>Convolution


@section{description}


Strict convolution with fixed kernel which can be updated using a trigger
signal.

Internally, this unit uses FFT to accelerate the calculation, which means that
(a) you must specify a "framesize", and
(b) if the kernel is longer than this framesize, the end of it will be ignored.

See also link::http://www.dspguide.com/ch18.htm:: by Steven W.
Smith.


@section{classmethods}
 

@section{method}
 ar

@section{argument}
 in

Processing target.


@section{argument}
 kernel

Buffer index for the fixed kernel, may be modulated in
combination with the trigger. It's size must be <= framesize.


@section{argument}
 trigger

Update the kernel on a change from non-positive to positive
value.


@section{argument}
 framesize

Size of FFT frame, must be a power of two (512, 1024, 2048, 4096 are standard choices).
Convolution uses twice this number internally.
Note that the convolution gets progressively more expensive to run for higher powers!
The maximum value you can use is 2^16=16384.
(This upper limit is half of "SC_FFT_MAXSIZE" defined in the SC source code.)
Larger convolutions than this can be done using link::Classes/PartConv::.

@section{argument}
 mul

@section{argument}
 add

@section{Examples}
 


@racketblock[
( // allocate three buffers
b = Buffer.alloc(s,2048);
c = Buffer.alloc(s,2048);
d = Buffer.alloc(s,2048);

b.zero;
c.zero;
d.zero;
)

(
50.do({ |it| c.set(20*it+10, 1.0.rand); });
3.do({ |it| b.set(400*it+100, 1); });
20.do({ |it| d.set(40*it+20, 1); });
)


(
SynthDef( "conv-test", { arg kernel, trig=0;
	var input;

	input=Impulse.ar(1);

	//must have power of two framesize
	Out.ar(0,Convolution2.ar(input,kernel,trig,2048, 0.5));
}).add

)


x = Synth.new("conv-test",[\kernel,b.bufnum]);

// changing the buffer number:
x.set(\kernel,c.bufnum);
x.set(\trig,0);
x.set(\trig,1); // after this trigger, the change will take effect.
x.set(\kernel,d.bufnum);
x.set(\trig,0);
x.set(\trig,1); // after this trigger, the change will take effect.

d.zero;
40.do({ |it| d.set(20*it+10, 1); });// changing the buffers' contents
x.set(\trig,0);
x.set(\trig,1); // after this trigger, the change will take effect.

x.set(\kernel,b.bufnum);
x.set(\trig,0);
x.set(\trig,1); // after this trigger, the change will take effect.


// next example
b = Buffer.read(s, Platform.resourceDir +/+ "sounds/a11wlk01.wav");

(
	{ var input, kernel;

	input=SoundIn.ar(0);

	//must have power of two framesize
	Out.ar(0,Convolution2.ar(input,b.bufnum,0,512, 0.5));
	 }.play;

)


// another example
(
//must have power of two framesize- FFT size will be sorted by Convolution2 to be double this
//maximum is currently a=8192 for FFT of size 16384
a=2048;
s = Server.local;
//kernel buffer
g = Buffer.alloc(s,a,1);
)

(
g.set(0,1.0);
100.do({arg i; g.set(a.rand, (i+1).reciprocal)});
)

(
// random impulse response

	{
	var input,inputAmp,threshhold,gate;

input = SoundIn.ar(0);
inputAmp = Amplitude.kr(input);
threshhold = 0.02;	// noise gating threshold
gate = Lag.kr(inputAmp > threshhold, 0.01);

	Out.ar(0,Convolution2.ar(input*gate,g.bufnum,0, a, 0.5));
	 }.play;

)

// one last example
(
b = Buffer.alloc(s, 512, 1);
b.sine1(1.0/[1,2,3,4,5,6], true, true, true);
)

(
	{ var input, kernel;

	input=SoundIn.ar(0);

	//must have power of two framesize
	Out.ar(0,Convolution2.ar(input,b.bufnum,0, 512, 0.5));
	 }.play;

)
::

Instead of triggering the kernel update yourself, as in the first example, you can use a UGen trigger signal to do so. In the next example, we use two Convolution2 UGens in order to continuously and smoothly change the impulse response: link::Classes/RecordBuf:: is used to record a random frequency link::Classes/Saw:: oscillator every ]

@racketblock[trigPeriod:: seconds.
Right after the recording (trigPeriod gets delayed by the buffer duration link::Classes/BufDur::, using the link::Classes/TDelay:: UGen) the two convolution UGens alternatively update their kernels (using two triggers convTrigs). At the frequency of the kernel updates a crossfader link::Classes/XFade2:: moves between conv1 and conv2, using a triangle oscillator link::Classes/LFTri:: at half the trigger frequency as a panning input. The result is a constantly shifting spectral colorization of the Dust impulses:

]

@racketblock[
b = Buffer.alloc( s, 2048, 1, _.zeroMsg );
(
	x = { arg i_kernel, density = 100, trigPeriod = 5.0, cutOff = 1000, minFreq = 200, maxFreq = 2000;
		var input, trigFreq, recTrig, irSig, convTrig, convTrigs, bufFrames, conv1, conv2;

		input		= LPF.ar( Dust2.ar( density ), cutOff );
		trigFreq		= trigPeriod.reciprocal;
		recTrig		= Impulse.kr( trigFreq );
		irSig		= Saw.ar( TExpRand.kr( minFreq, maxFreq, recTrig ), 0.4 );
		RecordBuf.ar( irSig, i_kernel, recTrig, loop: 0, trigger: recTrig );
		convTrig		= TDelay.kr( recTrig, BufDur.ir( i_kernel ));
		// split updates across two triggers. Note that [ 1, 0 ] creates
		// a MultiChannel expansion!
		convTrigs		= PulseDivider.kr( convTrig, 2, [ 1, 0 ]);
		bufFrames		= BufFrames.ir( i_kernel );
		// create the two alternatingly updated convolution ugens
		#conv1, conv2	= Convolution2.ar( input, i_kernel, convTrigs, bufFrames );

		XFade2.ar( conv1, conv2, LFTri.kr( trigFreq * 0.5, 1 )) ! 2;
	}.play( s, [ \i_kernel, b ]);
)

x.set( \trigPeriod, 0.1 );	// fast changes
x.set( \trigPeriod, 10.0 );	// slow changes
x.free;	// delete synth
::

]


