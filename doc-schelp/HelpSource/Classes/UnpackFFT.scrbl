#lang scribble/manual
@(require (for-label racket))

@title{UnpackFFT}
 Unpack an FFT chain into separate demand-rate FFT bin streams@section{categories}
  UGens>FFT
@section{related}
  Classes/PackFFT, Classes/Unpack1FFT

@section{description}

Takes an FFT chain and separates the magnitude and phase data into separate demand-rate streams, for arithmetic manipulation etc.

This is technically a demand-rate UGen. The actual "demand" is usually created by PackFFT later on in the graph, which requests the values in order to re-pack the data. This allows for processing to occur in between...

See also pvcollect, pvcalc and pvcalc2 methods ( in link::Classes/PV_ChainUGen:: ) which provide convenient ways to process audio in the frequency domain. The help for pvcollect includes notes on efficiency considerations.

@section{classmethods}
 
@section{private}
  categories

@section{method}
  new
@section{argument}
  chain
FFT chain
@section{argument}
  bufsize
FFT buffer size
@section{argument}
  frombin
limiting analysis to the bins of interest
@section{argument}
  tobin
limiting analysis to the bins of interest
@section{returns}
 
A list from DC up to Nyquist of 
@racketblock[ [mag[0], phase[0], mag[1], phase[1], ... mag[nyquist], phase[nyquist]]. ::
]
@section{discussion}
 
Note that you do have to decide your FFT buffer size in advance, since this determines how many values the UGen will output.

@racketblock[
#magsphases = UnpackFFT(chain, bufsize)
::

]
@section{examples}
 

@racketblock[
(
s.waitForBoot({
	var fftsize = 1024;
	b = Buffer.alloc(s, fftsize, 1);
	c = Buffer.read(s, Platform.resourceDir +/+ "sounds/a11wlk01.wav");
})
)

// This one just drags out various the values and posts them - a little bit pointless!
(
x = {
	var sig, chain, unp;
	sig = SinOsc.ar;
	sig = PlayBuf.ar(1, c, BufRateScale.kr(c), loop: 1);
	chain = FFT(b, sig);

	// Using the frombin & tobin args makes it much more efficient, limiting analysis to the bins of interest
	unp = UnpackFFT(chain, b.numFrames, frombin: 0, tobin: 4);

	// Demand some data from the unpacker.
	// NOTE: At present, Demand.kr is unable to handle more than 32 inputs,
	// so using frombin & tobin to limit the number of bins is compulsory.
	Demand.kr(chain>=0, 0, unp).collect{|anunp, index|
		anunp.poll(chain>=0, if(index % 2 == 0,  "Magnitude", "Phase")+(index/2).floor);
	};

	(sig*0.1).dup;
}.play(s);
)
x.free;

// Now a simple frequency-domain manipulation, square-rooting the magnitudes AND phases.
(
x = {
	var in, chain, magsphases;
	in = PlayBuf.ar(1, c, BufRateScale.kr(c), loop: 1);
	chain = FFT(b, in);
	magsphases = UnpackFFT(chain, b.numFrames);
	magsphases = magsphases.collect(_.sqrt);
	PackFFT(chain, b.numFrames, magsphases);
	Out.ar(0, 0.25 * IFFT(chain).dup);
}.play(s);
)
x.free;
::

]


