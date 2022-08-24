#lang scribble/manual
@(require (for-label racket))

@title{PartConv}
 Real-time partitioned convolution@section{categories}
  UGens>FFT, UGens>Convolution
@section{related}
  Classes/Convolution, Classes/Convolution2, Classes/Convolution2L, Classes/Convolution3

@section{description}

Partitioned convolution. Various additional buffers must be supplied.

Mono impulse response only! If inputting multiple channels, you'll need independent PartConvs, one for each channel.

But the charm is: impulse response can be as large as you like (CPU load increases with IR size. Various tradeoffs based on fftsize choice, due to rarer but larger FFTs. This plug-in uses amortisation to spread processing and avoid spikes).

Normalisation factors difficult to anticipate; convolution piles up multiple copies of the input on top of itself, so can easily overload.

@section{classmethods}
 
@section{method}
  ar

@section{argument}
  in
processing target.
@section{argument}
  fftsize
spectral convolution partition size (twice partition size). You must ensure that the blocksize divides the partition size and there are at least two blocks per partition (to allow for amortisation).
@section{argument}
  irbufnum
prepared buffer of spectra for each partition of the inpulse response.
@section{argument}
  mul
@section{argument}
  add

@section{examples}
 

@racketblock[
// preparation; essentially, allocate an impulse response buffer, then follow a special buffer preparation step to set up the data the plugin needs. Different options are provided commented out for loading impulse responses from soundfiles.
(
~fftsize=2048; // also 4096 works on my machine; 1024 too often and amortisation too pushed, 8192 more high load FFT

s.waitForBoot({

{
var ir, irbuffer, bufsize;

	// // MONO ONLY
	// pre-existing impulse response sound files
	// (could also use any general soundfile too for cross-synthesis effects)
	// irbuffer= Buffer.read(s, "/Volumes/data/audio/ir/ir2.wav");
	// irbuffer= Buffer.read(s, "/Volumes/data/audio/ir/ir.wav");
	// this is a two second long hall IR
	// irbuffer= Buffer.read(s, "/Volumes/data/audio/ir/bighall2.wav");


	// synthesise the honourable 'Dan Stowell' impulse response

	ir = ([1] ++0.dup(100) ++ ((1, 0.99998 .. 0).collect{|f| f =
	f.squared.squared; f = if(f.coin){0}{f.squared}; f =
	if(0.5.coin){0-f}{f} } * 0.1)).normalizeSum;
	// ir.plot;

	irbuffer = Buffer.loadCollection(s, ir);

	s.sync;

	bufsize= PartConv.calcBufSize(~fftsize, irbuffer);

	// ~numpartitions= PartConv.calcNumPartitions(~fftsize, irbuffer);

	~irspectrum= Buffer.alloc(s, bufsize, 1);

	~irspectrum.preparePartConv(irbuffer, ~fftsize);

	s.sync;

	irbuffer.free; // don't need time domain data anymore, just needed spectral version
}.fork;

});

)



~target= Buffer.read(s, Platform.resourceDir +/+ "sounds/a11wlk01.wav");
// ~target= Buffer.read(s, "sounds/break");

(

{ var input, kernel;

input= PlayBuf.ar(1, ~target, loop:1);

Out.ar(0, PartConv.ar(input, ~fftsize, ~irspectrum.bufnum, 0.5));
 }.play;

)


// convolve with live input
(

{ var input, kernel;

input= SoundIn.ar(0);

Out.ar(0, PartConv.ar(input, ~fftsize, ~irspectrum.bufnum));
}.play;
)


// should get back original impulse response (once every four seconds)
(

{ var input, kernel;

input= Impulse.ar(0.25);

Out.ar(0, PartConv.ar(input, ~fftsize, ~irspectrum.bufnum));
 }.play;

)


// only free buffers once you're finished with examples
// if you free whilst PartConv is still running, the server won't crash, but PartConv output will go to zero abruptly
(
~irspectrum.free;
~target.free;
currentEnvironment.clear;
)
::
]


