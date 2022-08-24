#lang scribble/manual
@(require (for-label racket))

@title{Unpack1FFT}
 Unpack a single value (magnitude or phase) from an FFT chain@section{categories}
  UGens>FFT
@section{related}
  Classes/PackFFT, Classes/UnpackFFT


Unpack1FFT(chain, bufsize, binindex, whichmeasure=0)

@section{description}

Takes an FFT chain and extracts a single scalar value as a demand-rate stream. To call it you need a "demander" which fires whenever the FFT chain fires - this is normally achieved using link::Classes/PackFFT:: but can also be done using link::Classes/Demand::.

@section{Note}
 
The main purpose of this UGen is as a component in pvcollect, pvcalc, and pvcalc2 processes. You're welcome to use it on its own - the example below shows basic usage. But most people won't typically need to use it directly.
::

@section{classmethods}
 
@section{private}
  categories

@section{method}
  new

@section{argument}
  chain
an FFT chain
@section{argument}
  bufsize
the size of the expected input FFT frames
@section{argument}
  binindex
the integer index of the bin you want to query
@section{argument}
  whichmeasure
0 for magnitude and 1 for phase. None of these arguments can be modulated.

@section{examples}
 

@racketblock[
(
s.waitForBoot({
	c = Buffer.read(s, Platform.resourceDir +/+ "sounds/a11wlk01.wav");
})
)

// Let's extract the DC component - i.e. the magnitude at index zero.
(
x = {
	var fftsize = 1024;
	var sig, chain, unp;
	sig = PlayBuf.ar(1, c, BufRateScale.kr(c), loop: 1);
	chain = FFT(LocalBuf(fftsize), sig);

	unp = Unpack1FFT(chain, b.numFrames, 0, 0);

	// Demand some data from the unpacker
	Demand.kr(chain>=0, 0, unp).poll(chain>=0, "unpacked value");

	(sig*0.1).dup;
}.play(s);
)
x.free;
::

]


